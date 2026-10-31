"""Self-tests of how the runner starts sessions.

The expectations come from plan 085: a session's git reads only the
runner's configuration, never the repository of the caller; when a
session stops, every process it started stops too; and a run that dies,
cleanly or not, leaves no process behind.  Plan 102 adds one: a session
compiles nothing just in time.  Plan 106 another: a session that waits
past its deadline names the spec it waits in, and what it waits for.
"""

from __future__ import annotations

import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from typing import TYPE_CHECKING
from unittest import mock

from runner import accounting, sessions

if TYPE_CHECKING:
    from collections.abc import Callable

SCENARIO = "l1-session-first-file"  # one short scenario file, as a filter
WARNING_SCENARIO = "l1-session-warning-gui"  # its first spec logs a warning
MACOS_TMPDIR = 48  # the length of a standard macOS TMPDIR, /var/folders/xx/<30>/T
HANG_FIXTURE = sessions.TESTS / "runner" / "fixtures" / "hang-in-prompt.el"
HANG_SECONDS = 15  # a session reaches its specs in about 5 s, even under load

# What git exports to a hook that runs in a linked worktree.
HOOK_GIT = {
    "GIT_DIR": "/repo/.git/worktrees/wt",
    "GIT_INDEX_FILE": "/repo/.git/worktrees/wt/index",
    "GIT_WORK_TREE": "/repo",
    "GIT_EDITOR": ":",
    "GIT_CONFIG_GLOBAL": "/caller/.gitconfig",
}


class PrepareTest(unittest.TestCase):
    """The environment a session starts with."""

    def setUp(self) -> None:
        """Give each test a fresh directory for its session."""
        self.directory = Path(tempfile.mkdtemp(prefix="prepare-"))
        self.addCleanup(shutil.rmtree, self.directory)

    def test_git_reads_only_the_runner_configuration(self) -> None:
        """A caller's GIT_* variables, as a git hook sets them, never reach git.

        Git also finds no live straight.el repository: each one is a
        ceiling, so loading magit cannot take a lock there.
        """
        repositories = self.directory / "live" / "straight" / "repos"
        for name in ("alpha", "beta"):
            (repositories / name / ".git").mkdir(parents=True)
        with (mock.patch.dict(os.environ, HOOK_GIT),
              mock.patch.object(sessions, "LIVE", self.directory / "live")):
            environment = sessions.prepare(self.directory / "s", None).environment
        self.assertEqual(
            {name: value for name, value in environment.items()
             if name.startswith("GIT_")},
            {"GIT_CONFIG_GLOBAL": str(sessions.GITCONFIG), "GIT_CONFIG_NOSYSTEM": "1",
             "GIT_CEILING_DIRECTORIES": os.pathsep.join(
                 [str(repositories / "alpha"), str(repositories / "beta")])})

    def test_a_session_reaches_no_session_bus(self) -> None:
        """Plan 106: the owner's D-Bus would give it the desktop's theme."""
        environment = sessions.prepare(self.directory / "s", None).environment
        result = subprocess.run(
            [sessions.EMACS, "--batch", "--eval",
             ("(progn (require 'dbus nil t) (princ (and (featurep 'dbusbind)"
              ' (dbus-ping :session "org.freedesktop.DBus"))))')],
            env=environment, capture_output=True, text=True, timeout=60, check=True)
        self.assertEqual(result.stdout, "nil")

    def test_a_session_compiles_nothing_just_in_time(self) -> None:
        """Once the configuration's early-init.el has run, JIT compilation is off.

        That early-init.el turns it on; the session's own early-init.el
        must have the last word.
        """
        session = self.directory / "s"
        environment = sessions.prepare(session, None).environment
        result = subprocess.run(
            [sessions.EMACS, "--batch", "-l", str(session / "init" / "early-init.el"),
             "--eval", "(princ native-comp-jit-compilation)"],
            env=environment, capture_output=True, text=True, timeout=60, check=True)
        self.assertEqual(result.stdout, "nil")

    def test_a_failing_configuration_leaves_the_harness_and_lifeline(self) -> None:
        """Plan 106: the configuration's early-init.el fails, as in neomacs.

        The session must still answer the runner's protocol and end with
        its runner: a daemon outlives an error in its early-init.
        """
        source = self.directory / "source"
        source.mkdir()
        (source / "early-init.el").write_text('(error "Boom in early-init")\n')
        session = self.directory / "s"
        with mock.patch.object(sessions, "SOURCE", source):
            environment = sessions.prepare(session, None).environment
        early_init = sessions.elisp_string(session / "init" / "early-init.el")
        result = subprocess.run(
            [sessions.EMACS, "--batch", "--eval",
             f"(condition-case nil (load {early_init} nil t t) (error nil))",
             "--eval", ("(princ (list (boundp 'my/test-session-started)"
                        " (length (seq-filter #'timer--repeat-delay timer-list))))")],
            env=environment, capture_output=True, text=True, timeout=60, check=True)
        self.assertEqual(result.stdout, "(t 1)")



def alive(pid: int) -> bool:
    """Return true when the process PID exists."""
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    return True


def wait_until(condition: Callable[[], bool], seconds: float) -> bool:
    """Poll CONDITION for up to SECONDS; return its last value."""
    deadline = time.monotonic() + seconds
    while not condition() and time.monotonic() < deadline:
        time.sleep(0.1)
    return condition()


class ReadLinesTest(unittest.TestCase):
    """The result lines a session wrote, as the runner reads them."""

    def test_only_newlines_end_lines(self) -> None:
        """A raw U+2028 in a failure text keeps its line, and the next, whole."""
        directory = Path(tempfile.mkdtemp(prefix="lines-"))
        self.addCleanup(shutil.rmtree, directory)
        path = directory / "results.jsonl"
        path.write_text('{"failure":"a\u2028b\u0085c"}\n{"type":"done"}\n{"cut',
                        encoding="utf-8")
        self.assertEqual(sessions.read_lines(path),
                         ['{"failure":"a\u2028b\u0085c"}', '{"type":"done"}'])


class DescendantsTest(unittest.TestCase):
    """Which processes a session started, from a process table."""

    def test_every_generation_below_the_process_and_nothing_else(self) -> None:
        """Children and grandchildren count; the parent and siblings do not."""
        rows = [(10, 1, "a"), (20, 10, "b"), (30, 20, "c"), (40, 1, "d"),
                (50, 40, "e"), (60, 10, "f")]
        self.assertEqual(sessions.descendants(10, rows),
                         {20: "b", 30: "c", 60: "f"})


class ReaperTest(unittest.TestCase):
    """Stopping a session stops what it started."""

    def test_a_child_in_its_own_session_dies_with_its_parent(self) -> None:
        """As Emacs starts a language server: own session, deaf to stdin."""
        directory = Path(tempfile.mkdtemp(prefix="reaper-"))
        self.addCleanup(shutil.rmtree, directory)
        session = sessions.prepare(directory / "s", None)
        script = ("import subprocess, time\n"
                  "child = subprocess.Popen(['sleep', '417'], start_new_session=True,\n"
                  "                         stdin=subprocess.DEVNULL)\n"
                  "print(child.pid, flush=True)\n"
                  "time.sleep(60)\n")
        reaper = sessions.Reaper()
        process = reaper.start([sys.executable, "-c", script], session)
        self.addCleanup(reaper.stop_all)
        self.assertTrue(wait_until(lambda: session.log.read_text().strip() != "", 10))
        child = int(session.log.read_text())
        self.addCleanup(lambda: alive(child) and os.kill(child, signal.SIGKILL))
        reaper.stop(process)
        self.assertTrue(wait_until(lambda: not alive(child), 5))

    def test_a_reaped_process_is_not_signalled(self) -> None:
        """Its PID, and the group of that PID, may belong to others by now."""
        directory = Path(tempfile.mkdtemp(prefix="reaper-"))
        self.addCleanup(shutil.rmtree, directory)
        reaper = sessions.Reaper()
        process = reaper.start([sys.executable, "-c", "pass"],
                               sessions.prepare(directory / "s", None))
        process.wait()
        with mock.patch.object(sessions.os, "killpg") as killpg, \
                mock.patch.object(sessions.os, "kill") as kill:
            reaper.stop(process)
        self.assertEqual((killpg.call_args_list, kill.call_args_list), ([], []))


@unittest.skipUnless(sessions.XVFB and os.environ.get("EMACS_TEST_GUI") != "0",
                     "needs graphical frames on Xvfb")
class WarningTest(unittest.TestCase):
    """Plan 105: a warning on a graphical frame ends nothing, and is shown."""

    def test_the_file_runs_on_and_the_runner_shows_the_warning(self) -> None:
        """Both specs pass, well before the deadline, and the warning is printed."""
        start = time.monotonic()
        result = subprocess.run(
            [sys.executable, "-m", "runner", "fast", WARNING_SCENARIO],
            cwd=sessions.TESTS, capture_output=True, text=True,
            timeout=sessions.SCENARIO_SECONDS + 60, check=False)
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertIn("2 passed", result.stdout)
        self.assertIn("warning SL-WARNING: ", result.stdout)
        self.assertIn("a probe warning", result.stdout)
        self.assertLess(time.monotonic() - start, sessions.SCENARIO_SECONDS / 2)


@unittest.skipUnless(sessions.XVFB and os.environ.get("EMACS_TEST_GUI") != "0",
                     "needs graphical frames on Xvfb")
class HangTest(unittest.TestCase):
    """Plan 106: a session that waits past its deadline says for what."""

    def test_a_spec_waiting_in_a_prompt_is_reported_with_it(self) -> None:
        """The spec is named, and the state shows the question asked."""
        root = Path(tempfile.mkdtemp(prefix="emacs-test-"))
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        scenario = sessions.Scenario(HANG_FIXTURE, accounting.Headers())
        with mock.patch.object(sessions, "SCENARIO_SECONDS", HANG_SECONDS):
            outcome = sessions.run_scenario(scenario, root / "00", sessions.Reaper(),
                                            graphic=True)
        self.assertEqual(outcome.error,
                         "deadline exceeded while waiting for the end of the specs")
        records, malformed = accounting.parse_report(outcome.lines)
        verdict = accounting.judge(records, outcome.files, outcome.socket,
                                   outcome.display)
        self.assertEqual(malformed, [])
        hung = "HANG waits for an answer nobody gives: started and never finished"
        self.assertIn(hung, verdict.problems)
        self.assertTrue(any("Restart the probe server?" in problem
                            for problem in verdict.problems), verdict.problems)


class LinkedTmpdirTest(unittest.TestCase):
    """Plan 106: under a linked TMPDIR, as macOS's /tmp is, files are found."""

    def test_a_first_file_scenario_passes_under_a_linked_tmpdir(self) -> None:
        """The runner finds the first file that the session visited."""
        base = Path(tempfile.mkdtemp(prefix="r", dir="/tmp"))
        self.addCleanup(shutil.rmtree, base)
        (base / "real").mkdir()
        (base / "link").symlink_to(base / "real")
        result = subprocess.run(
            [sys.executable, "-m", "runner", "fast", SCENARIO], cwd=sessions.TESTS,
            env={**os.environ, "TMPDIR": str(base / "link"), "EMACS_TEST_GUI": "0"},
            capture_output=True, text=True,
            timeout=sessions.SCENARIO_SECONDS + 60, check=False)
        self.assertEqual(result.returncode, 0, result.stdout)


class SignalledRunTest(unittest.TestCase):
    """A run stopped by a signal leaves no session and no display behind."""

    def test_a_hung_up_or_killed_run_leaves_no_process(self) -> None:
        """SIGHUP, as when the terminal closes; SIGKILL, as on a tool timeout.

        The killed run has no display: losing Xvfb would end its daemon
        too, and the daemon must end without that help.  test_display
        covers Xvfb itself.  Each run's TMPDIR is as long as a standard
        macOS one, whatever the caller's, so the socket paths of its
        sessions must fit there.
        """
        runs = []
        no_display = {"EMACS_TEST_GUI": "0"}
        for number, gui in ((signal.SIGHUP, {}), (signal.SIGKILL, no_display)):
            base = Path(tempfile.mkdtemp(prefix="r", dir="/tmp"))
            self.addCleanup(shutil.rmtree, base)
            directory = base / "T".rjust(MACOS_TMPDIR - len(str(base)) - 1, "x")
            directory.mkdir()
            with (base / "runner.log").open("wb") as log:
                runner = subprocess.Popen(
                    [sys.executable, "-m", "runner", "fast", SCENARIO],
                    cwd=sessions.TESTS,
                    env={**os.environ, "TMPDIR": str(directory), **gui},
                    stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT,
                    start_new_session=True)
            self.addCleanup(runner.wait)
            self.addCleanup(lambda run=runner: run.poll() is None and run.kill())
            runs.append((number, directory, runner))
        for number, directory, runner in runs:

            def session_up(here: Path = directory) -> bool:
                return any(here.glob("emacs-test-*/*/sock/test"))

            wait_until(lambda run=runner: session_up() or run.poll() is not None, 60)
            self.assertTrue(session_up(), "no session started; the runner said: "
                            + (directory.parent / "runner.log").read_text())
            family = sessions.descendants(runner.pid, sessions.process_rows())
            commands = subprocess.run(
                [sessions.PS, "-o", "args=", "-p", ",".join(map(str, family))],
                capture_output=True, text=True, check=False).stdout
            self.assertIn("--fg-daemon", commands)
            os.kill(runner.pid, number)
            runner.wait(timeout=30)

            def survivors(started: dict[int, str] = family) -> list[int]:
                return [pid for pid, _, start in sessions.process_rows()
                        if started.get(pid) == start]

            self.assertTrue(wait_until(lambda: not survivors(), 15),
                            f"{number!r} left {survivors()}")


if __name__ == "__main__":
    unittest.main()
