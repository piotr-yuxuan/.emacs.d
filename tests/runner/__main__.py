"""Run the tests of this Emacs configuration.

Usage: tests/run [static|fast|full [FILTER]]

static  checks that need no session: the T0 integrity checks of plan 086
        and their logic tests, in one `emacs --batch -Q`
        (tests/integrity/static/run.el).
fast    the harness self-tests, the logic tests and every scenario file
        that is not L3.  This is the default.
full    fast, plus the L3 scenario files (real language servers, tools),
        then the isolation check: each spec of a sample of scenario files
        runs alone in a fresh session and must keep its status.

FILTER, given after the profile, keeps the test files whose path under
tests/ contains it.

Each scenario file (tests/scenarios/) runs in its own fresh daemon, in a
client frame on its own private Xvfb display, which the user never sees
and no other session shares.  With
EMACS_TEST_GUI=0, no graphical frame is created, and files declaring
';; scenario-display: graphic' are expected skips.  Without Xvfb, or
with an Emacs that cannot draw on X (as on macOS), the run stops with
a usage error unless EMACS_TEST_GUI=0 is set.  A file declaring
';; scenario-tier: l3' runs only in full.  The logic tests and harness
self-tests (tests/**/*-test.el outside tests/scenarios/) share one
batch session; the Python self-tests of the runner
(tests/runner/test_*.py) run with unittest.

The exit status is 1 on any failure, load error, unexpected skip, stale
expected-skip entry or deadline, and 2 on a usage error.  The directory
of a green session is removed; the others are kept and printed.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import contextlib
import dataclasses
import functools
import os
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING

from runner import accounting, sessions

if TYPE_CHECKING:
    from collections.abc import Callable

PROFILES = ("static", "fast", "full")
LOGIC_SECONDS = 300
PYTHON_SECONDS = 120
STATIC_SECONDS = 120
WIDE_POOL = max(2, (os.cpu_count() or 2) // 2)
NARROW_POOL = max(1, (os.cpu_count() or 2) // 8)  # L3 files start language servers
ISOLATION_SAMPLE = 3  # scenario files whose specs full also runs one by one
NOT_LOGIC = frozenset({"scenarios", "fixtures", "tmp"})
TESTS = sessions.TESTS
STATIC = TESTS / "integrity" / "static" / "run.el"


@dataclasses.dataclass
class Plan:
    """The tests one run executes."""

    python_tests: list[Path]
    logic: list[Path]
    scenarios: list[sessions.Scenario]
    graphic: list[Path]  # scenario files skipped for want of a display
    problems: list[str]  # scenario files whose headers are invalid

    def empty(self) -> bool:
        """Return true when the plan runs nothing."""
        return not (self.python_tests or self.logic or self.scenarios or self.graphic
                    or self.problems)


def name(path: Path | str) -> str:
    """Return PATH relative to tests/, for display."""
    return str(path).replace(f"{TESTS}/", "")


def plan(profile: str, pattern: str, *, gui: bool) -> Plan:
    """Select the tests of PROFILE whose path under tests/ contains PATTERN."""

    def chosen(path: Path) -> bool:
        return pattern in path.relative_to(TESTS).as_posix()

    selected = Plan(
        [p for p in sorted((TESTS / "runner").glob("test_*.py")) if chosen(p)],
        [p for p in sorted(TESTS.rglob("*-test.el"))
         if p.relative_to(TESTS).parts[0] not in NOT_LOGIC and chosen(p)],
        [], [], [])
    for path in filter(chosen, sorted((TESTS / "scenarios").rglob("*.el"))):
        try:
            headers = accounting.parse_headers(path.read_text(encoding="utf-8"))
        except ValueError as error:
            selected.problems.append(f"{name(path)}: {error}")
            continue
        if headers.l3 and profile != "full":
            continue
        if headers.graphic and not gui:
            selected.graphic.append(path)
        else:
            selected.scenarios.append(sessions.Scenario(path, headers))
    return selected


def run_static(pattern: str) -> int:
    """Run the static tier on the files whose path contains PATTERN; return its status.

    One `emacs --batch -Q` loads tests/integrity/static/run.el.  It is
    stopped when it outlives STATIC_SECONDS or the run is interrupted.
    """
    process = subprocess.Popen(
        [sessions.EMACS, "--batch", "-Q", "-l", str(STATIC), pattern],
        stdin=subprocess.DEVNULL)
    try:
        return 1 if process.wait(timeout=STATIC_SECONDS) else 0
    except subprocess.TimeoutExpired:
        sys.stdout.write(f"FAIL  the static checks exceeded {STATIC_SECONDS} s\n")
        return 1
    finally:
        if process.poll() is None:
            process.terminate()
            with contextlib.suppress(subprocess.TimeoutExpired):
                process.wait(timeout=sessions.STOP_SECONDS)
            process.kill()
            process.wait()


def run_python_tests(paths: list[Path]) -> list[str]:
    """Run the runner's own unittest modules PATHS; return their problems."""
    modules = [f"runner.{path.stem}" for path in paths]
    try:
        result = subprocess.run(
            [sys.executable, "-m", "unittest", "-q", *modules], cwd=TESTS,
            capture_output=True, text=True, timeout=PYTHON_SECONDS, check=False)
    except subprocess.TimeoutExpired:
        return [f"the Python self-tests exceeded {PYTHON_SECONDS} s"]
    return [result.stderr.strip()] if result.returncode else []


@dataclasses.dataclass(frozen=True)
class Job:
    """One session to run, and how to name, pool and check it."""

    label: str
    run: Callable[[], sessions.Outcome]
    narrow: bool = False  # an L3 file: run it in the small pool
    scenario: sessions.Scenario | None = None
    expected: str | None = None  # a spec run alone: its status in its whole file


def report(label: str, future: concurrent.futures.Future[sessions.Outcome],
           ) -> accounting.Verdict:
    """Judge the session of FUTURE, print it under LABEL, remove it if green."""
    try:
        outcome = future.result()
    except (sessions.SessionError, sessions.InterruptedRunError, OSError) as error:
        sys.stdout.write(f"FAIL  {label}: {error}\n")
        return accounting.Verdict([str(error)], 0, [], set(), {}, {})
    records, malformed = accounting.parse_report(outcome.lines)
    verdict = accounting.judge(records, outcome.files, outcome.socket,
                               outcome.display)
    verdict.problems[:0] = ([outcome.error] if outcome.error else []) + malformed
    status = "FAIL" if verdict.problems else "ok"
    sys.stdout.write(f"{status:5} {label}: {verdict.passed} passed, "
                     f"{len(verdict.expected)} skipped ({outcome.seconds:.1f} s)\n")
    for problem in verdict.problems:
        sys.stdout.write(f"        {name(problem)}\n")
    if verdict.problems:
        sys.stdout.write(f"        kept {outcome.directory}\n")
    else:
        # A late writer must not crash the run; the root then stays.
        shutil.rmtree(outcome.directory, ignore_errors=True)
    sys.stdout.flush()
    return verdict


def execute(jobs: list[Job], reaper: sessions.Reaper,
            ) -> list[tuple[Job, accounting.Verdict]]:
    """Run JOBS in bounded pools; judge and print each session as it ends."""
    with concurrent.futures.ThreadPoolExecutor(WIDE_POOL) as wide, \
            concurrent.futures.ThreadPoolExecutor(NARROW_POOL) as narrow:
        futures = {(narrow if job.narrow else wide).submit(job.run): job
                   for job in jobs}
        try:
            return [(futures[future], report(futures[future].label, future))
                    for future in concurrent.futures.as_completed(futures)]
        except KeyboardInterrupt:
            for future in futures:
                future.cancel()
            reaper.stop_all()
            raise


def isolation_jobs(results: list[tuple[Job, accounting.Verdict]], root: Path,
                   reaper: sessions.Reaper, *, graphic: bool) -> list[Job]:
    """Return one job per spec of the sampled scenario files, run alone.

    The sample is the non-L3 scenario files with the most specs.  Each
    spec must have, alone in a fresh session, its status in its file.
    """
    sample = sorted(((job, verdict) for job, verdict in results
                     if job.scenario and not job.narrow and len(verdict.statuses) > 1),
                    key=lambda result: (-len(result[1].statuses), result[0].label))
    jobs = []
    for job, verdict in sample[:ISOLATION_SAMPLE]:
        for spec, status in sorted(verdict.statuses.items()):
            alone = dataclasses.replace(job.scenario, only=spec)
            directory = root / f"a{len(jobs):02d}"
            run = functools.partial(sessions.run_scenario, alone, directory, reaper,
                                    graphic=graphic)
            jobs.append(Job(f"{job.label} alone: {spec}", run, scenario=alone,
                            expected=status))
    return jobs


def summarize(profile: str, selected: Plan, verdicts: list[accounting.Verdict],
              alone: int, problems: list[str]) -> int:
    """Print the skips, the problems and the totals of a run; return its status.

    ALONE is the number of sessions the isolation check of full ran.
    """
    skips = [f"{name(p)}: needs a graphical display (EMACS_TEST_GUI=0)"
             for p in selected.graphic]
    skips += [s for v in verdicts for s in v.expected]
    for skip in skips:
        sys.stdout.write(f"skip  {skip}\n")
    for problem in problems:
        sys.stdout.write(f"FAIL  {name(problem)}\n")
    failing = sum(bool(v.problems) for v in verdicts)
    isolation = f", {alone} run alone" if profile == "full" else ""
    sys.stdout.write(
        f"{profile}: {len(verdicts)} sessions{isolation},"
        f" {sum(v.passed for v in verdicts)} specs passed, {len(skips)} expected"
        f" skips, {failing} failing sessions, {len(problems)} other problems.\n")
    return 1 if failing or problems else 0


def main(arguments: list[str]) -> int:
    """Run the profile and filter named by ARGUMENTS; return the exit status."""
    parser = argparse.ArgumentParser(
        prog="tests/run", description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("profile", nargs="?", choices=PROFILES, default="fast")
    parser.add_argument("filter", nargs="?", default="")
    options = parser.parse_args(arguments)
    profile, pattern = options.profile, options.filter
    if profile == "static":
        return run_static(pattern)
    if not (sessions.LIVE / "straight/repos/straight.el/bootstrap.el").exists():
        sys.stderr.write(f"No straight.el installation in {sessions.LIVE}; start"
                         " Emacs once normally, or set EMACS_LIVE_DIR.\n")
        return 1
    try:
        graphic = sessions.choose_display(os.environ.get("EMACS_TEST_GUI"),
                                          sessions.emacs_features(), sessions.XVFB)
    except (sessions.UsageError, subprocess.SubprocessError, OSError) as error:
        sys.stderr.write(f"tests/run: {error}.\n")
        return 2
    selected = plan(profile, pattern, gui=graphic)
    if selected.empty():
        sys.stderr.write(f"No {profile} test matches {pattern!r}.\n")
        return 1
    problems = list(selected.problems)
    if selected.python_tests:
        found = run_python_tests(selected.python_tests)
        sys.stdout.write(f"{'FAIL' if found else 'ok':5} runner self-tests (Python)\n")
        problems += found
    root = Path(tempfile.mkdtemp(prefix="emacs-test-"))
    reaper = sessions.Reaper()
    logic = functools.partial(sessions.run_logic, selected.logic, root / "logic",
                              reaper, seconds=LOGIC_SECONDS)
    jobs = [Job("logic and self-tests", logic)] if selected.logic else []
    jobs += [Job(name(scenario.path),
                 functools.partial(sessions.run_scenario, scenario,
                                   root / f"{index:02d}",
                                   reaper, graphic=graphic),
                 narrow=scenario.headers.l3, scenario=scenario)
             for index, scenario in enumerate(selected.scenarios)]
    try:
        results = execute(jobs, reaper)
        alone = (execute(isolation_jobs(results, root, reaper, graphic=graphic), reaper)
                 if profile == "full" else [])
    except KeyboardInterrupt:
        sys.stderr.write(f"Interrupted; session directories kept in {root}\n")
        return 130
    verdicts = [verdict for _, verdict in results]
    problems += [f"{job.label}: {verdict.statuses.get(job.scenario.only)} alone,"
                 f" {job.expected} in its file"
                 for job, verdict in alone
                 if verdict.statuses.get(job.scenario.only) != job.expected]
    problems += [f"{job.label}: {problem}"
                 for job, verdict in alone for problem in verdict.problems]
    complete = profile == "full" and not pattern and graphic
    problems += accounting.registry_problems(verdicts, complete=complete)
    status = summarize(profile, selected, verdicts, len(alone), problems)
    try:
        if status == 0:
            root.rmdir()
    except OSError:
        # Seen once under load in an L3 run: a short-lived process wrote
        # into a session directory after the runner had removed it.
        sys.stdout.write(f"note  something wrote into {root} after its session"
                         " ended; kept\n")
    return status


if __name__ == "__main__":
    # From before the first process starts, stop cleanly on Ctrl-C
    # (SIGINT), on kill (SIGTERM) and when the terminal closes (SIGHUP).
    signal.signal(signal.SIGTERM, signal.default_int_handler)
    signal.signal(signal.SIGHUP, signal.default_int_handler)
    sys.exit(main(sys.argv[1:]))
