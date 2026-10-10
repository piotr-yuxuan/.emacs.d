"""Check that the T1 driver calls the runner of plan 085 as the runner defines it.

The driver runs t1.el in sessions of 085's runner, which none of the
tests/run profiles runs.  When the runner's interface changes under it,
these tests fail in tests/run fast (through driver-test.el) instead of
the next manual T1 run.  The runner's session function is replaced by
a mock specced from the real one, so a call with the wrong arguments
fails, and no Emacs starts.
"""

from __future__ import annotations

import contextlib
import io
import os
import shutil
import tempfile
import unittest
from pathlib import Path
from typing import TYPE_CHECKING
from unittest import mock

from runner import __main__ as runner_main
from runner import sessions

from integrity.session import driver

if TYPE_CHECKING:
    from collections.abc import Iterator


class DriverTest(unittest.TestCase):
    """The driver's calls into the runner, checked against its signatures."""

    @contextlib.contextmanager
    def fake_runner(self, writes: str | None = None,
                    ) -> Iterator[tuple[mock.Mock, list[tuple[str, bool]]]]:
        """Run the driver against the real signatures, without sessions.

        Yield the mock of `sessions.run_scenario', then, per call, the
        name of the scenario file and whether the fixture local.el was
        in the session's init directory.  The live straight tree holds the
        repositories alpha and beta; each session writes to the .git
        directory of the one named WRITES, as git does for an index.lock.
        """
        seen: list[tuple[str, bool]] = []

        def run_scenario(scenario: sessions.Scenario, directory: Path,
                         *_: object, **__: object) -> mock.Mock:
            seen.append((scenario.path.name,
                         (directory / "init" / "local.el").is_file()))
            if writes:
                (repositories / writes / ".git" / "index.lock").touch()
            shutil.rmtree(directory)  # as the runner does for a green session
            return mock.Mock()

        def execute(jobs: list[runner_main.Job], _reaper: sessions.Reaper,
                    ) -> list[tuple[runner_main.Job, mock.Mock]]:
            for job in jobs:
                job.run()
            return [(job, mock.Mock(problems=[])) for job in jobs]

        with tempfile.TemporaryDirectory() as temporary, \
                mock.patch.object(sessions, "run_scenario", autospec=True,
                                  side_effect=run_scenario) as run, \
                mock.patch.object(driver, "execute", autospec=True,
                                  side_effect=execute), \
                mock.patch.object(driver.tempfile, "mkdtemp",
                                  return_value=str(Path(temporary) / "root")), \
                mock.patch.object(sessions, "LIVE", Path(temporary) / "live"), \
                mock.patch.object(driver.signal, "signal"), \
                contextlib.redirect_stdout(io.StringIO()), \
                contextlib.redirect_stderr(io.StringIO()):
            (Path(temporary) / "root").mkdir()
            repositories = Path(temporary) / "live" / "straight" / "repos"
            for name in ("alpha", "beta"):
                (repositories / name / ".git").mkdir(parents=True)
                os.utime(repositories / name / ".git", ns=(0, 0))
            yield run, seen

    def test_check_runs_both_files_with_the_fixture(self) -> None:
        """Each T1 file runs in a session whose init holds local.el."""
        with self.fake_runner() as (run, seen):
            self.assertEqual(driver.main(["check"], gui=True), 0)
        self.assertEqual(seen, [("t1.el", True), ("t1-gui.el", True)])
        self.assertEqual({call.kwargs["graphic"] for call in run.call_args_list}, {True})

    def test_check_skips_the_graphical_file_without_a_display(self) -> None:
        """Without a display, t1-gui.el is not run at all."""
        with self.fake_runner() as (_, seen):
            self.assertEqual(driver.main(["check"], gui=False), 0)
        self.assertEqual(seen, [("t1.el", True)])

    def test_a_write_to_a_live_repository_fails_the_run(self) -> None:
        """A run whose sessions change a live repository's .git fails."""
        with self.fake_runner(writes="beta") as (_, seen):
            self.assertEqual(driver.main(["check"], gui=False), 1)
        self.assertEqual(seen, [("t1.el", True)])


if __name__ == "__main__":
    unittest.main()
