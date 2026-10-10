"""Check the mutation check of tests/mutate.

The expectations come from the tool's contract: the edits of a mutant
apply only where they fit, the mutant list holds the 31 mutants of the
085 baseline and one mutant per journey of plan 094, every mutant fits
the current init.el and names tests that exist, the verdicts follow
the documented table, each run happens in a copy that is removed, and
the runner's own output is read as the runner of plan 085 prints it.
"""

from __future__ import annotations

import concurrent.futures
import contextlib
import io
import json
import os
import re
import shutil
import tempfile
import time
import unittest
from pathlib import Path
from typing import TYPE_CHECKING
from unittest import mock

from runner import __main__ as runner_main
from runner import sessions

from mutation import __main__ as tool
from mutation.mutants import MUTANTS, Mutant, journey

if TYPE_CHECKING:
    from collections.abc import Iterator

BASELINE = sessions.SOURCE / "docs" / "work" / "findings" / "085-mutation-baseline.org"
JOURNEYS = range(1, 15)
MACOS_TMPDIR = 48  # /var/folders/xx/<30>/T, as in runner/test_sessions.py
# The longest directory tests/run nests in its TMPDIR for a session's
# sockets: its root, the logic session, the sockets (runner/__main__.py).
RUNNER_SOCKETS = "/emacs-test-XXXXXXXX/logic/sock"

FAKE_RUN = """\
#!/bin/sh
# A stand-in for tests/run: it reports what it sees, and fails, as
# tests/run prints a failed session, when init.el holds BROKEN or the
# filter is "red".
cd "$(dirname "$0")/.." || exit 2
printf 'init: %s\\n' "$(cat init.el)"
printf 'args: %s\\n' "$*"
printf 'tmpdir: %s\\n' "$TMPDIR"
[ -e early-init.el ] && echo "early-init present"
[ -e tests/tmp ] && echo "tmp copied"
[ -e tests/mutation ] && echo "mutation copied"
[ -L tests/fixtures/link ] && echo "link kept"
touch "$TMPDIR/written"
if grep -q BROKEN init.el || [ "$2" = red ]; then
  echo "FAIL  scenarios/l1-a.el: 0 passed, 0 skipped (1.0 s)"
  echo "        J-01: a spec [row]: failed"
  echo "        kept /tmp/somewhere/00"
  echo "fast: 1 sessions, 0 specs passed, 0 expected skips, 1 failing sessions."
  exit 1
fi
echo "fast: 1 sessions, 1 specs passed, 0 expected skips, 0 failing sessions."
"""


@contextlib.contextmanager
def fake_checkout(script: str = FAKE_RUN) -> Iterator[Path]:
    """Yield a checkout whose tests/run is SCRIPT, as the tool's SOURCE.

    Its tests/ holds a tmp/ and a mutation/ directory, which a copy
    must leave out, and a dangling symbolic link, which it must keep.
    """
    with tempfile.TemporaryDirectory() as directory:
        source = Path(directory) / "checkout"
        for path in ("tests/tmp", "tests/mutation", "tests/fixtures"):
            (source / path).mkdir(parents=True)
        (source / "early-init.el").write_text(";; early\n")
        (source / "init.el").write_text("good\n")
        (source / "tests" / "fixtures" / "link").symlink_to("../nowhere")
        (source / "tests" / "run").write_text(script)
        (source / "tests" / "run").chmod(0o755)
        with mock.patch.object(tool, "SOURCE", source):
            yield Path(directory)


def run_result(status: int | None, *problems: str) -> tool.Run:
    """Return a Run with STATUS and PROBLEMS."""
    return tool.Run(status, problems, "summary", 1.0)


class MutateTest(unittest.TestCase):
    """Edits apply in turn, and only where their old text is unique."""

    def test_edits_apply_in_turn(self) -> None:
        """A later edit sees the text the earlier ones made."""
        self.assertEqual(tool.mutate("a b c", [("b", "x"), ("x c", "y")]), "a y")

    def test_an_edit_that_does_not_fit_gives_nothing(self) -> None:
        """Missing, repeated, or removed by an earlier edit: no mutant."""
        self.assertIsNone(tool.mutate("a b c", [("z", "x")]))
        self.assertIsNone(tool.mutate("a b b", [("b", "x")]))
        self.assertIsNone(tool.mutate("a b c", [("b", "x"), ("b", "y")]))


class MutantListTest(unittest.TestCase):
    """The mutant list against the baseline, the journeys and init.el."""

    @unittest.skipUnless(BASELINE.parents[1].is_dir(), "docs/work is not checked out")
    def test_it_holds_the_31_baseline_mutants(self) -> None:
        """The K and U mutants are those of the 085 baseline table."""
        baseline = re.findall(r"^\| ([KU]\d\d-[a-z0-9-]+) +\|",
                              BASELINE.read_text(encoding="utf-8"), re.MULTILINE)
        self.assertEqual(len(baseline), 31)
        self.assertEqual([m.ident for m in MUTANTS if m.ident[0] in "KU"], baseline)

    def test_one_mutant_per_journey_expects_its_spec_to_fail(self) -> None:
        """Each journey has one mutant, run on its file, expecting J-NN to fail."""
        journeys = [m for m in MUTANTS if m.ident.startswith("J")]
        self.assertEqual([(m.scope, m.expected) for m in journeys],
                         [(("full", f"journey-{n:02d}-"), f"J-{n:02d}:")
                          for n in JOURNEYS])
        self.assertEqual(len({m.ident for m in MUTANTS}), len(MUTANTS))

    def test_every_mutant_fits_the_current_init_el(self) -> None:
        """Each edit's old text occurs exactly once, and the mutant changes init.el."""
        original = (sessions.SOURCE / "init.el").read_text(encoding="utf-8")
        misfits = [m.ident for m in MUTANTS
                   if tool.mutate(original, m.edits) in {None, original}]
        self.assertEqual(misfits, [])

    def test_every_targeted_run_has_its_test(self) -> None:
        """A mutant's filter selects scenario files that define its expected spec."""
        problems = []
        for mutant in MUTANTS:
            profile, pattern = mutant.scope
            if not pattern:
                continue
            selected = runner_main.plan(profile, pattern, gui=True).scenarios
            files = [scenario.path for scenario in selected]
            defines = [f':describe "{mutant.expected}' in path.read_text("utf-8")
                       for path in files]
            if not files or (mutant.expected and defines != [True]):
                problems.append((mutant.ident, [path.name for path in files]))
        self.assertEqual(problems, [])


class VerdictTest(unittest.TestCase):
    """The verdict table of the tool's documentation."""

    def test_the_verdicts(self) -> None:
        """Each verdict, from the control, the status and the failed specs."""
        baseline = Mutant("K99-x", "x", ())
        third = journey(3, "x", "x", ())
        green, red = run_result(0), run_result(1, "J-03: a spec [row]: failed")
        cases = [
            (baseline, run_result(1), green, "KILLED"),
            (baseline, green, green, "SURVIVED"),
            (baseline, run_result(1), red, "VOID"),
            (baseline, run_result(2), green, "ERROR"),
            (baseline, run_result(None), green, "ERROR"),
            (third, red, green, "KILLED"),
            (third, run_result(1, "J-01: another [row]: failed"), green, "OFF-TARGET"),
            (third, run_result(1, "deadline exceeded"), green, "OFF-TARGET"),
        ]
        self.assertEqual([tool.verdict(m, run, ctl) for m, run, ctl, _ in cases],
                         [expected for *_, expected in cases])


class RunnerOutputTest(unittest.TestCase):
    """The problems and summary are read as the runner of plan 085 prints them."""

    def test_a_failed_spec_and_the_summary(self) -> None:
        """The runner's own report and summary give the failed spec's name."""
        socket = "/sessions/00/sock/test"
        lines = [json.dumps(record) for record in (
            {"type": "spec", "file": "j.el", "name": "J-03: a search [search]",
             "status": "failed", "disabled": False, "failure": "no match\nsecond line",
             "commands": [], "skip_entries": []},
            {"type": "skip-entries", "entries": []},
            {"type": "done", "server_socket": socket, "editor": f"x {socket}",
             "visual": f"x {socket}", "emacs_socket_name": None, "display": None,
             "wayland_display": None, "frame_displays": []})]
        output = io.StringIO()
        with tempfile.TemporaryDirectory() as directory, \
                contextlib.redirect_stdout(output):
            future: concurrent.futures.Future[sessions.Outcome] = (
                concurrent.futures.Future())
            future.set_result(sessions.Outcome(["j.el"], lines, None, socket, None,
                                               Path(directory), 1.0))
            verdict = runner_main.report("scenarios/j.el", future)
            runner_main.summarize("full", runner_main.Plan([], [], [], [], []),
                                  [verdict], 0, [])
        run = tool.parse_output(output.getvalue(), 1, 1.0)
        self.assertEqual(run.problems, ("J-03: a search [search]: no match",))
        self.assertTrue(run.summary.startswith("full: 1 sessions"))
        self.assertEqual(tool.verdict(journey(3, "x", "x", ()), run, run_result(0)),
                         "KILLED")

    def test_a_session_that_could_not_start(self) -> None:
        """The runner prints why at the margin; that reason is a problem too."""
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            future: concurrent.futures.Future[sessions.Outcome] = (
                concurrent.futures.Future())
            future.set_exception(sessions.SessionError("socket directory too long"))
            verdict = runner_main.report("scenarios/j.el", future)
            runner_main.summarize("fast", runner_main.Plan([], [], [], [], []),
                                  [verdict], 0, [])
        run = tool.parse_output(output.getvalue(), 1, 1.0)
        self.assertEqual(run.problems, ("scenarios/j.el: socket directory too long",))


class RunsTest(unittest.TestCase):
    """Each run happens in a copy of its own, removed afterwards."""

    def test_a_run_sees_its_copy_and_leaves_only_its_log(self) -> None:
        """The copy holds the mutated init.el; TMPDIR is in it; only the log stays."""
        with fake_checkout() as directory:
            root = directory / "root"
            root.mkdir()
            run = tool.Runs(root).run("BROKEN", ("full", "journey-03-"), root / "m.log")
            log = (root / "m.log").read_text()
            self.assertEqual(list(root.iterdir()), [root / "m.log"])
        self.assertEqual(run.status, 1)
        self.assertEqual(run.problems, ("J-01: a spec [row]: failed",))
        self.assertIn("init: BROKEN\nargs: full journey-03-\n"
                      f"tmpdir: {root / 'r00'}\nearly-init present\nlink kept\n", log)

    def test_a_run_past_its_deadline_is_stopped(self) -> None:
        """A tests/run that outlives RUN_SECONDS is stopped, without a status."""
        with fake_checkout("#!/bin/sh\nexec sleep 60\n") as directory, \
                mock.patch.object(tool, "RUN_SECONDS", 1):
            started = time.monotonic()
            run = tool.Runs(directory).run("good", ("fast", ""), directory / "m.log")
        self.assertIsNone(run.status)
        self.assertLess(time.monotonic() - started, 10)
        self.assertEqual(tool.verdict(Mutant("K99-x", "x", ()), run, run_result(0)),
                         "ERROR")


class MainTest(unittest.TestCase):
    """The whole check, on a fake checkout and mutant list."""

    def test_the_table_and_the_exit_status(self) -> None:
        """Every verdict reaches the table; a covered survivor or a misfit fails."""
        fakes = (
            Mutant("A-killed", "a", (("good", "BROKEN"),)),
            Mutant("B-survivor", "b", (("good", "fine"),), covered=False),
            Mutant("C-misfit", "c", (("absent", "x"),)),
            Mutant("D-off", "d", (("good", "BROKEN"),), scope=("full", "x"),
                   expected="J-09:"),
            Mutant("E-void", "e", (("good", "BROKEN"),), scope=("full", "red")),
        )
        output = io.StringIO()
        with fake_checkout() as directory, \
                mock.patch.object(tool, "MUTANTS", fakes), \
                mock.patch.object(tool.tempfile, "mkdtemp",
                                  return_value=str(directory / "root")), \
                contextlib.redirect_stdout(output):
            (directory / "root").mkdir()
            status = tool.main([])
            kept = sorted(path.name for path in (directory / "root").iterdir())
        table = {line.split(" | ")[0][2:]: line.split(" | ")[4]
                 for line in output.getvalue().splitlines() if line.startswith("| ")}
        self.assertEqual(status, 1)
        self.assertEqual(table, {"Mutant": "Verdict", "A-killed": "KILLED",
                                 "B-survivor": "SURVIVED", "C-misfit": "INVALID",
                                 "D-off": "OFF-TARGET", "E-void": "VOID"})
        self.assertIn("| VOID | control: J-01: a spec [row]: failed |",
                      output.getvalue())
        self.assertEqual(kept, ["A-killed.log", "B-survivor.log", "D-off.log",
                                "E-void.log", "control-fast.log",
                                "control-full-red.log", "control-full-x.log"])

    def test_the_killed_and_the_uncovered_survivors_pass(self) -> None:
        """Only KILLED, and SURVIVED where the suite claims nothing, give 0."""
        fakes = (Mutant("A-killed", "a", (("good", "BROKEN"),)),
                 Mutant("B-survivor", "b", (("good", "fine"),), covered=False))
        with fake_checkout() as directory, mock.patch.object(tool, "MUTANTS", fakes), \
                mock.patch.object(tool.tempfile, "mkdtemp",
                                  return_value=str(directory / "root")), \
                contextlib.redirect_stdout(io.StringIO()), \
                contextlib.redirect_stderr(io.StringIO()):
            (directory / "root").mkdir()
            self.assertEqual(tool.main(["A", "B"]), 0)
            self.assertEqual(tool.main(["Z"]), 2)

    def test_the_sockets_of_a_run_fit_under_a_long_tmpdir(self) -> None:
        """Under a TMPDIR as long as macOS's, every session's sockets still fit."""
        base = Path(tempfile.mkdtemp(prefix="r", dir="/tmp"))
        self.addCleanup(shutil.rmtree, base)
        long = base / "T".rjust(MACOS_TMPDIR - len(str(base)) - 1, "x")
        long.mkdir()
        output = io.StringIO()
        mutants = (Mutant("A", "a", (("good", "x"),)),)
        with fake_checkout(), mock.patch.object(tool, "MUTANTS", mutants), \
                mock.patch.dict(os.environ, {"TMPDIR": str(long)}), \
                mock.patch.object(tempfile, "tempdir", str(long)), \
                contextlib.redirect_stdout(output):
            tool.main([])
        root = Path(re.findall(r"^Logs: (.+)$", output.getvalue(), re.MULTILINE)[0])
        self.addCleanup(shutil.rmtree, root)
        logs = "".join(log.read_text() for log in root.glob("*.log"))
        given = re.findall(r"^tmpdir: (.+)$", logs, re.MULTILINE)
        self.assertEqual(len(given), 2)
        self.assertEqual([len(os.fsencode(tmpdir + RUNNER_SOCKETS))
                          <= sessions.SOCKET_DIRECTORY_LIMIT for tmpdir in given],
                         [True, True])


if __name__ == "__main__":
    unittest.main()
