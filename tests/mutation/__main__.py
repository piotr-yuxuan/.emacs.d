"""Mutation-test the suite: check that tests/run catches regressions of init.el.

Usage: tests/mutate [PREFIX...]

Run the mutants of tests/mutation/mutants.py whose identifier starts
with one of the PREFIXes, or all of them.  Each run gets its own copy
of init.el (mutated), early-init.el and tests/ (without the tool's own
tests/mutation/) in a temporary directory under /tmp, and runs the
copy's tests/run with TMPDIR inside the copy, so whatever the run
writes stays there; its sessions get private displays as in any
tests/run.  /tmp rather than the caller's TMPDIR keeps the sessions'
socket paths within the Unix limit: macOS's TMPDIR alone takes 48 of
its 104 bytes.  A baseline mutant (K, U) runs the whole fast profile,
or, when only an L3 test can catch it, `tests/run full FILTER` on that
test's file.  A journey mutant (J) runs `tests/run full journey-NN-`,
its journey's scenario file alone.  Runs go in parallel.

The unmutated copy also runs, once per profile and filter: the
control.  Verdicts:

  KILLED      tests/run failed; for a journey mutant, its J-NN spec failed
  SURVIVED    tests/run passed
  OFF-TARGET  a journey mutant's run failed, but not its J-NN spec
  VOID        the control run of the same profile and filter failed
  INVALID     an edit's old text does not occur exactly once in init.el
  ERROR       tests/run stopped otherwise: usage error, interrupt, deadline

The report is an Org table on stdout, with the first problems of each
run, or of the control of a VOID mutant: a kill by a spec unrelated to
the area, such as a missed deadline, may come from the load of the
machine; run that mutant again alone.  The log of each run stays in
the temporary directory, which is printed.  The exit status is 0 when
every mutant the suite claims to cover is KILLED and every other one
KILLED or SURVIVED, 1 otherwise, and 2 on a usage error.  tests/run
never runs this tool: a whole run takes a long time.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import contextlib
import dataclasses
import itertools
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path
from typing import TYPE_CHECKING

from mutation.mutants import MUTANTS, Mutant

if TYPE_CHECKING:
    from collections.abc import Sequence

SOURCE = Path(__file__).resolve().parents[2]  # the checkout whose suite is tested
# Each tests/run starts sessions on half the CPUs.  Three runs at once on
# 16 CPUs pushed a fast session past its 120 s deadline and reordered
# the timers of a scenario-language spec: kills that were only load.
PARALLEL_RUNS = max(1, (os.cpu_count() or 2) // 8)
RUN_SECONDS = 3600
STOP_SECONDS = 30  # tests/run stops its sessions on SIGTERM; give it the time
# A problem: under a failed session, or at the margin, such as a session
# that could not start, but not the tally of a failed session.
PROBLEM = re.compile(r" {8}(?!kept /)(\S.*)"
                     r"|FAIL  (?!.*: \d+ passed, \d+ skipped \()(.*)")
SUMMARY = re.compile(r"(?:fast|full): \d+ sessions.*")
DETAIL_CHARACTERS = 90

Scope = tuple[str, str]  # the profile and FILTER of a tests/run


@dataclasses.dataclass(frozen=True)
class Run:
    """What one tests/run on a copy gave."""

    status: int | None  # its exit status; None when it did not start or end in time
    problems: tuple[str, ...]  # the problems it printed under failed sessions
    summary: str  # its summary line, or what went wrong
    seconds: float


def mutate(text: str, edits: Sequence[tuple[str, str]]) -> str | None:
    """Return TEXT with each (OLD, NEW) of EDITS applied in turn.

    Return None when an OLD does not occur exactly once in the text it
    applies to: the mutant no longer fits this init.el.
    """
    for old, new in edits:
        if text.count(old) != 1:
            return None
        text = text.replace(old, new)
    return text


def label(scope: Scope) -> str:
    """Return the arguments of tests/run for SCOPE, as one string."""
    return " ".join(filter(None, scope))


def parse_output(text: str, status: int | None, seconds: float) -> Run:
    """Return the Run of a tests/run that printed TEXT and exited with STATUS."""
    lines = text.splitlines()
    problems = tuple(match[1] or match[2]
                     for match in map(PROBLEM.fullmatch, lines) if match)
    summaries = [line for line in lines if SUMMARY.fullmatch(line)]
    summary = summaries[-1] if summaries else (lines[-1] if lines else "no output")
    return Run(status, problems, summary, seconds)


def verdict(mutant: Mutant, run: Run, control: Run) -> str:
    """Return the verdict on MUTANT from its RUN and the CONTROL run of its scope."""
    if control.status != 0:
        return "VOID"
    if run.status == 0:
        return "SURVIVED"
    if run.status != 1:
        return "ERROR"
    expected = mutant.expected
    if expected is None or any(p.startswith(expected) for p in run.problems):
        return "KILLED"
    return "OFF-TARGET"


class Runs:
    """Own the copies and the tests/run processes of one mutation check."""

    def __init__(self, root: Path) -> None:
        """Make copies in ROOT, which the caller owns."""
        self._root = root
        self._lock = threading.Lock()
        self._numbers = itertools.count()
        self._running: set[subprocess.Popen[bytes]] = set()
        self._closed = False

    def run(self, init: str, scope: Scope, log: Path) -> Run:
        """Run tests/run SCOPE on a copy whose init.el is INIT; output goes to LOG.

        tests/run gets RUN_SECONDS, then is stopped.  The copy is removed
        afterwards, whatever happens; LOG stays.
        """
        started = time.monotonic()
        with self._lock:
            copy = self._root / f"r{next(self._numbers):02d}"
        try:
            copy.mkdir()
            shutil.copy2(SOURCE / "early-init.el", copy)
            (copy / "init.el").write_text(init, encoding="utf-8")
            ignored = shutil.ignore_patterns("tmp", "__pycache__", "mutation")
            shutil.copytree(SOURCE / "tests", copy / "tests", symlinks=True,
                            ignore=ignored)
            with log.open("wb") as output, self._lock:
                if self._closed:
                    return Run(None, (), "not started: the check is stopping", 0.0)
                # A session of its own: on Ctrl-C, only this tool's SIGTERM
                # reaches tests/run, which then stops its sessions.
                process = subprocess.Popen(
                    [str(copy / "tests" / "run"), *filter(None, scope)],
                    stdin=subprocess.DEVNULL, stdout=output, stderr=subprocess.STDOUT,
                    env={**os.environ, "TMPDIR": str(copy)}, start_new_session=True)
                self._running.add(process)
            status = None
            try:
                with contextlib.suppress(subprocess.TimeoutExpired):
                    status = process.wait(timeout=RUN_SECONDS)
            finally:
                process.terminate()
                with contextlib.suppress(subprocess.TimeoutExpired):
                    process.wait(timeout=STOP_SECONDS)
                process.kill()
                process.wait()
                with self._lock:
                    self._running.discard(process)
        except OSError as error:
            return Run(None, (), f"could not run: {error}", time.monotonic() - started)
        finally:
            shutil.rmtree(copy, ignore_errors=True)
        run = parse_output(log.read_text(encoding="utf-8", errors="replace"),
                           status, time.monotonic() - started)
        if status is None:
            return dataclasses.replace(run, summary=f"stopped after {RUN_SECONDS} s")
        return run

    def stop_all(self) -> None:
        """Start no more runs, and ask every running tests/run to stop."""
        with self._lock:
            self._closed = True
            running = list(self._running)
        for process in running:
            process.terminate()


def report(rows: list[tuple[Mutant, str, str]]) -> str:
    """Return the Org table of ROWS, each (MUTANT, VERDICT, DETAIL)."""
    lines = ["| Mutant | Area | Covered | Run | Verdict | Detail |", "|-"]
    for mutant, outcome, detail in rows:
        cells = (mutant.ident, mutant.area, "yes" if mutant.covered else "no",
                 label(mutant.scope), outcome, detail[:DETAIL_CHARACTERS])
        lines.append("| " + " | ".join(c.replace("|", "\\vert{}") for c in cells)
                     + " |")
    return "\n".join(lines) + "\n"


def main(arguments: list[str]) -> int:
    """Run the mutants ARGUMENTS name, print the table; return the exit status."""
    parser = argparse.ArgumentParser(
        prog="tests/mutate", description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("prefixes", nargs="*", metavar="PREFIX")
    prefixes = tuple(parser.parse_args(arguments).prefixes)
    unknown = [p for p in prefixes if not any(m.ident.startswith(p) for m in MUTANTS)]
    if unknown:
        sys.stderr.write(f"tests/mutate: no mutant starts with {', '.join(unknown)}\n")
        return 2
    chosen = [m for m in MUTANTS if not prefixes or m.ident.startswith(prefixes)]
    original = (SOURCE / "init.el").read_text(encoding="utf-8")
    jobs: dict[str, tuple[str, Scope]] = {}
    for mutant in chosen:
        text = mutate(original, mutant.edits)
        if text is not None:
            jobs[mutant.ident] = (text, mutant.scope)
            jobs[f"control {label(mutant.scope)}"] = (original, mutant.scope)
    root = Path(tempfile.mkdtemp(prefix="emacs-mut-", dir="/tmp"))
    runs = Runs(root)
    results: dict[str, Run] = {}
    # The whole profile first: the longest runs start first.
    order = sorted(jobs, key=lambda name: jobs[name][1][1] != "")
    with concurrent.futures.ThreadPoolExecutor(PARALLEL_RUNS) as pool:
        futures = {pool.submit(runs.run, *jobs[name],
                               root / f"{name.replace(' ', '-')}.log"): name
                   for name in order}
        try:
            for future in concurrent.futures.as_completed(futures):
                run = results[futures[future]] = future.result()
                sys.stdout.write(f"done  {futures[future]} ({run.seconds:.0f} s):"
                                 f" {run.summary}\n")
                sys.stdout.flush()
        except KeyboardInterrupt:
            pool.shutdown(wait=False, cancel_futures=True)
            runs.stop_all()
            sys.stderr.write(f"Interrupted; logs kept in {root}\n")
            return 130
    rows = []
    for mutant in chosen:
        run = results.get(mutant.ident)
        control = results.get(f"control {label(mutant.scope)}")
        if run is None or control is None:
            rows.append((mutant, "INVALID", "an edit does not occur exactly once"))
            continue
        outcome = verdict(mutant, run, control)
        shown, before = (control, "control: ") if outcome == "VOID" else (run, "")
        rows.append((mutant, outcome,
                     before + ("; ".join(shown.problems[:2]) or shown.summary)))
    sys.stdout.write("\n" + report(rows) + f"\nLogs: {root}\n")
    good = [v == "KILLED" or (v == "SURVIVED" and not m.covered) for m, v, _ in rows]
    return 0 if all(good) else 1


if __name__ == "__main__":
    # Stop the runs cleanly on Ctrl-C, kill and hangup, as tests/run does.
    signal.signal(signal.SIGTERM, signal.default_int_handler)
    signal.signal(signal.SIGHUP, signal.default_int_handler)
    sys.exit(main(sys.argv[1:]))
