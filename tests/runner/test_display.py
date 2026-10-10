"""Self-tests of the runner's display: test frames never reach the user's screen.

The expectations come from the requirement that a test run leaves the
user's display alone (decision D5: GUI frames, with EMACS_TEST_GUI=0 as
the one switch that removes them), that a run never drops its
graphical scenarios silently, that the private display dies with the
run, and that sessions are isolated from one another (plan 085, one
fresh daemon per scenario file).
"""

from __future__ import annotations

import concurrent.futures
import os
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from unittest import mock

from runner import accounting, sessions

PS = shutil.which("ps") or "/bin/ps"
USER = {"DISPLAY": ":0", "WAYLAND_DISPLAY": "wayland-0"}
PGTK = frozenset({"PGTK", "GTK3", "NATIVE_COMP"})
GTK_X11 = frozenset({"X11", "GTK3", "XIM"})
NS = frozenset({"NS", "NATIVE_COMP"})  # an Emacs for macOS
XQUARTZ_XVFB = "/opt/X11/bin/Xvfb"
NO_FRAMES = os.environ.get("EMACS_TEST_GUI") == "0" or not sessions.XVFB
# Two sessions run at the same time: the owner sets the clipboard, then
# waits until the reader has read its own clipboard.
OWNER = """\
;;; owner.el --- Owns the clipboard of its display -*- lexical-binding: t -*-
(describe "The owner"
  (it "holds the clipboard until the reader has looked"
    (gui-set-selection 'CLIPBOARD "clipboard of the owner")
    (write-region "" nil {owned} nil 'silent)
    (let ((deadline (+ (float-time) 30)))
      (while (and (not (file-exists-p {read})) (< (float-time) deadline))
        (sit-for 0.05)))
    (expect (file-exists-p {read}) :to-be t)))
"""
READER = """\
;;; reader.el --- Reads the clipboard of its display -*- lexical-binding: t -*-
(describe "The reader"
  (it "does not see the clipboard of the owner"
    (let ((deadline (+ (float-time) 30)))
      (while (and (not (file-exists-p {owned})) (< (float-time) deadline))
        (sit-for 0.05)))
    (expect (file-exists-p {owned}) :to-be t)
    (let ((seen (gui-get-selection 'CLIPBOARD)))
      (write-region "" nil {read} nil 'silent)
      (expect seen :not :to-equal "clipboard of the owner"))))
"""


def children(parent: int) -> dict[int, str]:
    """Return the command line of each process whose parent is PARENT."""
    table = subprocess.run([PS, "-A", "-o", "pid=,ppid=,args="],
                           capture_output=True, text=True, check=True).stdout
    rows = (line.split(None, 2) for line in table.splitlines())
    return {int(pid): args for pid, ppid, args in rows if int(ppid) == parent}


def alive(pid: int) -> bool:
    """Return true when the process PID exists."""
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    return True


class ChooseDisplayTest(unittest.TestCase):
    """Whether a run gets a private display, decided before anything starts."""

    def test_the_switch_removes_every_frame_whatever_the_host(self) -> None:
        """EMACS_TEST_GUI=0 runs without frames, even where none could exist."""
        for features, xvfb in ((PGTK, "/usr/bin/Xvfb"), (NS, None), (GTK_X11, None)):
            self.assertFalse(sessions.choose_display("0", features, xvfb))

    def test_an_x_capable_emacs_with_xvfb_gets_a_private_display(self) -> None:
        """PGTK (on the X11 backend) and X11 builds can draw on Xvfb."""
        for features in (PGTK, GTK_X11):
            self.assertTrue(sessions.choose_display(None, features, "/usr/bin/Xvfb"))
            self.assertTrue(sessions.choose_display("", features, "/usr/bin/Xvfb"))

    def test_without_xvfb_the_run_stops_instead_of_skipping(self) -> None:
        """A missing Xvfb would drop the graphical scenarios: it is an error."""
        with self.assertRaisesRegex(sessions.UsageError, "EMACS_TEST_GUI=0"):
            sessions.choose_display(None, PGTK, None)

    def test_an_emacs_that_draws_only_on_the_screen_never_gets_frames(self) -> None:
        """An NS Emacs ignores DISPLAY, so XQuartz's Xvfb cannot hide its frames."""
        with self.assertRaisesRegex(sessions.UsageError, "EMACS_TEST_GUI=0"):
            sessions.choose_display(None, NS, XQUARTZ_XVFB)

    def test_an_unknown_switch_value_is_rejected(self) -> None:
        """Only 0 turns frames off; other values are not guessed at."""
        for value in ("false", "off", "1", "screen"):
            with self.assertRaisesRegex(sessions.UsageError, repr(value)):
                sessions.choose_display(value, PGTK, "/usr/bin/Xvfb")


class SessionDisplayTest(unittest.TestCase):
    """The display a prepared session sees comes from the runner alone."""

    def setUp(self) -> None:
        """Give each test a fresh directory, under the user's own displays."""
        self.directory = Path(tempfile.mkdtemp(prefix="display-"))
        self.addCleanup(shutil.rmtree, self.directory)
        patcher = mock.patch.dict(os.environ, USER)
        patcher.start()
        self.addCleanup(patcher.stop)

    def test_without_a_display_a_session_inherits_none(self) -> None:
        """No DISPLAY and no WAYLAND_DISPLAY of the user reach the session."""
        environment = sessions.prepare(self.directory / "s", None).environment
        self.assertNotIn("DISPLAY", environment)
        self.assertNotIn("WAYLAND_DISPLAY", environment)

    def test_with_the_private_display_a_session_sees_only_it(self) -> None:
        """The session's only display is the one the runner gives it."""
        environment = sessions.prepare(self.directory / "s", ":7").environment
        self.assertEqual(environment["DISPLAY"], ":7")
        self.assertNotIn("WAYLAND_DISPLAY", environment)


@unittest.skipIf(NO_FRAMES, "no graphical frame: EMACS_TEST_GUI=0, or no Xvfb")
class SessionClipboardTest(unittest.TestCase):
    """Sessions running at the same time share no X selection."""

    def test_a_session_never_sees_the_clipboard_of_another(self) -> None:
        """A kill in one session must not reach a yank in another."""
        directory = Path(tempfile.mkdtemp(prefix="clip-"))
        self.addCleanup(shutil.rmtree, directory)
        markers = {"owned": sessions.elisp_string(directory / "owned"),
                   "read": sessions.elisp_string(directory / "read")}
        (directory / "owner.el").write_text(OWNER.format(**markers))
        (directory / "reader.el").write_text(READER.format(**markers))
        reaper = sessions.Reaper()
        self.addCleanup(reaper.stop_all)

        def run(name: str) -> sessions.Outcome:
            scenario = sessions.Scenario(directory / f"{name}.el",
                                         accounting.Headers(graphic=True))
            return sessions.run_scenario(scenario, directory / name[0], reaper,
                                         graphic=True)

        with concurrent.futures.ThreadPoolExecutor(2) as pool:
            outcomes = list(pool.map(run, ("owner", "reader")))
        for outcome in outcomes:
            records, malformed = accounting.parse_report(outcome.lines)
            verdict = accounting.judge(records, outcome.files, outcome.socket,
                                       outcome.display)
            self.assertEqual((outcome.error, malformed, verdict.problems),
                             (None, [], []))
            self.assertEqual(verdict.passed, 1)


class WatchdogTest(unittest.TestCase):
    """The watchdog stops the display only once the run is gone."""

    def test_a_failing_ps_leaves_the_server_running(self) -> None:
        """Without a parent PID from ps, the run may be alive: keep its display."""
        directory = Path(tempfile.mkdtemp(prefix="watchdog-"))
        self.addCleanup(shutil.rmtree, directory)
        failing = directory / "ps"
        failing.write_text("#!/bin/sh\nexit 1\n")
        failing.chmod(0o755)
        server = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
        self.addCleanup(server.wait)
        self.addCleanup(server.kill)
        watchdog = subprocess.Popen(
            [sessions.SH, "-c", sessions.WATCHDOG, "watchdog", str(server.pid)],
            env={**os.environ, "PATH": f"{directory}{os.pathsep}{os.environ['PATH']}"})
        self.addCleanup(watchdog.wait)
        self.addCleanup(watchdog.kill)
        time.sleep(2.5)  # two rounds of the watchdog
        self.assertIsNone(server.poll())
        self.assertIsNone(watchdog.poll())


@unittest.skipUnless(sessions.XVFB, "Xvfb is not installed")
class PrivateDisplayTest(unittest.TestCase):
    """The private display is an X server owned by the run."""

    def test_the_private_display_is_a_fresh_server_gone_afterwards(self) -> None:
        """Inside, one new X server of this process; afterwards, nothing started."""
        before = set(children(os.getpid()))
        with sessions.private_display() as display:
            self.assertRegex(display, r"\A:\d+\Z")
            self.assertNotEqual(display, USER["DISPLAY"])
            started = {pid: args for pid, args in children(os.getpid()).items()
                       if pid not in before}
            servers = [pid for pid, args in started.items() if "-displayfd" in args]
            self.assertEqual(len(servers), 1)
        self.assertEqual([pid for pid in started if alive(pid)], [])

    def test_the_server_stops_when_the_run_is_killed(self) -> None:
        """A runner killed with SIGKILL cannot clean up; its Xvfb still stops."""
        script = ("import time\n"
                  "from runner import sessions\n"
                  "with sessions.private_display() as display:\n"
                  "    print(display, flush=True)\n"
                  "    time.sleep(60)\n")
        runner = subprocess.Popen([sys.executable, "-c", script],
                                  cwd=sessions.TESTS, stdout=subprocess.PIPE, text=True)
        self.addCleanup(runner.stdout.close)
        self.addCleanup(runner.wait)
        self.addCleanup(runner.kill)
        self.assertRegex(runner.stdout.readline(), r"\A:\d+\n\Z")
        servers = [pid for pid, args in children(runner.pid).items()
                   if "-displayfd" in args]
        self.assertEqual(len(servers), 1)
        runner.kill()
        deadline = time.monotonic() + 5
        while alive(servers[0]) and time.monotonic() < deadline:
            time.sleep(0.1)
        self.assertFalse(alive(servers[0]))


if __name__ == "__main__":
    unittest.main()
