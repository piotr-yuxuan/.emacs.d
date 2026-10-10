"""Self-tests of the runner's choices: what a run executes and its verdict.

The expectations come from plan 085: fast runs every scenario file that
is not L3 and full adds L3; without graphical frames, a graphic file is
an expected skip, never a session; full runs each spec of the three
non-L3 files with the most specs alone, and each must keep its status;
any problem makes the run fail.
"""

from __future__ import annotations

import contextlib
import io
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import runner.__main__ as run
from runner import accounting, sessions

FILES = {
    "runner/test_example.py": "",
    "helpers/example-test.el": "",
    "fixtures/project/fixture-test.el": "",
    "scenarios/l1-plain.el": ";;; l1-plain.el --- A\n",
    "scenarios/l1-window.el": ";;; l1-window.el --- A\n;; scenario-display: graphic\n",
    "scenarios/l3-server.el": ";;; l3-server.el --- A\n;; scenario-tier: l3\n",
    "scenarios/l1-broken.el": ";;; l1-broken.el --- A\n;; scenario-tier: l2\n",
}


def verdict(statuses: dict[str, str], problems: tuple[str, ...] = (),
            ) -> accounting.Verdict:
    """Return the verdict of a session whose specs ended with STATUSES."""
    passed = sum(status == "passed" for status in statuses.values())
    return accounting.Verdict(list(problems), passed, [], set(), {}, statuses)


def never_run() -> sessions.Outcome:
    """Stand for a session that the code under test must not start."""
    message = "a session was started"
    raise AssertionError(message)


def scenario_job(file: str, *, narrow: bool = False) -> run.Job:
    """Return the job of the scenario file FILE, as a run builds it."""
    headers = accounting.Headers(l3=narrow)
    return run.Job(file, never_run, narrow=narrow,
                   scenario=sessions.Scenario(Path(file), headers))


class PlanTest(unittest.TestCase):
    """The tests a profile selects, in a scratch tests/ tree."""

    def setUp(self) -> None:
        """Write FILES under a scratch tests/ directory and select from it."""
        self.tests = Path(tempfile.mkdtemp(prefix="plan-"))
        self.addCleanup(shutil.rmtree, self.tests)
        for file, text in FILES.items():
            (self.tests / file).parent.mkdir(parents=True, exist_ok=True)
            (self.tests / file).write_text(text)
        patcher = mock.patch.object(run, "TESTS", self.tests)
        patcher.start()
        self.addCleanup(patcher.stop)

    def selected(self, profile: str, pattern: str = "", *, gui: bool = True,
                 ) -> dict[str, list[str]]:
        """Return the plan of PROFILE for PATTERN, as paths under tests/."""
        chosen = run.plan(profile, pattern, gui=gui)
        return {
            "python": [run.name(p) for p in chosen.python_tests],
            "logic": [run.name(p) for p in chosen.logic],
            "scenarios": [run.name(s.path) for s in chosen.scenarios],
            "graphic": [run.name(p) for p in chosen.graphic],
            "problems": chosen.problems,
        }

    def test_fast_leaves_out_l3_and_skips_graphic_files_without_frames(self) -> None:
        """No L3 file in fast; a graphic file is a skip, not a session."""
        self.assertEqual(self.selected("fast", gui=False), {
            "python": ["runner/test_example.py"],
            "logic": ["helpers/example-test.el"],
            "scenarios": ["scenarios/l1-plain.el"],
            "graphic": ["scenarios/l1-window.el"],
            "problems": [("scenarios/l1-broken.el: unknown, repeated or malformed"
                          " header: ;; scenario-tier: l2")],
        })

    def test_full_with_frames_runs_l3_and_graphic_files(self) -> None:
        """Full adds L3; with frames, a graphic file runs like any other."""
        selected = self.selected("full", gui=True)
        self.assertEqual(
            (selected["scenarios"], selected["graphic"]),
            (["scenarios/l1-plain.el", "scenarios/l1-window.el",
              "scenarios/l3-server.el"], []))

    def test_the_filter_keeps_only_matching_paths(self) -> None:
        """A filter selects by path under tests/, in every kind of test."""
        self.assertEqual(self.selected("full", "l3-"), {
            "python": [], "logic": [], "scenarios": ["scenarios/l3-server.el"],
            "graphic": [], "problems": [],
        })


class IsolationJobsTest(unittest.TestCase):
    """The specs full runs alone, and the status each must keep."""

    def test_each_spec_of_the_three_largest_non_l3_files_runs_alone(self) -> None:
        """L3 and single-spec files are left out; ties go by name."""
        results = [
            (run.Job("logic", never_run),
             verdict({f"l{i}": "passed" for i in range(9)})),
            (scenario_job("l3-big.el", narrow=True),
             verdict({f"b{i}": "passed" for i in range(8)})),
            (scenario_job("one.el"), verdict({"o": "passed"})),
            (scenario_job("two.el"), verdict({"t1": "passed", "t2": "failed"})),
            (scenario_job("three.el"),
             verdict({"h1": "passed", "h2": "pending", "h3": "passed"})),
            (scenario_job("other-two.el"), verdict({"w1": "passed", "w2": "passed"})),
            (scenario_job("four.el"), verdict({f"f{i}": "passed" for i in range(4)})),
        ]
        jobs = run.isolation_jobs(results, Path("/run"), sessions.Reaper(),
                                  graphic=True)
        expected = {("four.el", f"f{i}", "passed") for i in range(4)}
        expected |= {("three.el", "h1", "passed"), ("three.el", "h2", "pending"),
                     ("three.el", "h3", "passed"),
                     ("other-two.el", "w1", "passed"), ("other-two.el", "w2", "passed")}
        self.assertEqual(
            {(str(job.scenario.path), job.scenario.only, job.expected) for job in jobs},
            expected)
        self.assertEqual(len(jobs), len(expected))
        self.assertEqual({job.run.keywords["graphic"] for job in jobs}, {True})


class SummarizeTest(unittest.TestCase):
    """The exit status and the totals of a run."""

    def summarize(self, verdicts: list[accounting.Verdict], problems: list[str],
                  profile: str = "fast", alone: int = 0) -> tuple[int, str]:
        """Return the status and the output of summarizing a run."""
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            status = run.summarize(profile, run.Plan([], [], [], [], []), verdicts,
                                   alone, problems)
        return status, output.getvalue()

    def test_a_failing_session_fails_the_run(self) -> None:
        """One problem in one session is enough."""
        status, _ = self.summarize([verdict({"a": "passed"}),
                                    verdict({"b": "failed"}, ("b: why",))], [])
        self.assertEqual(status, 1)

    def test_a_problem_outside_sessions_fails_the_run(self) -> None:
        """For example a stale expected-skip entry, or a spec that differs alone."""
        status, _ = self.summarize([verdict({"a": "passed"})], ["a stale entry"])
        self.assertEqual(status, 1)

    def test_a_clean_run_passes(self) -> None:
        """No session problem and no other problem."""
        status, _ = self.summarize([verdict({"a": "passed"})], [])
        self.assertEqual(status, 0)

    def test_full_says_how_many_sessions_ran_alone(self) -> None:
        """A missing isolation check shows in the totals."""
        _, output = self.summarize([verdict({"a": "passed"})], [], "full", 33)
        self.assertIn("full: 1 sessions, 33 run alone,", output)


if __name__ == "__main__":
    unittest.main()
