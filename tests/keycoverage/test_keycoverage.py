"""Check the key-binding coverage report of tests/coverage.

The expectations come from the report's contract: a binding counts as
exercised only when a passed spec ran its command from its key (with
the documented exceptions), the table is plan 086's with the live
keymap's last-binding-wins rule, and the sessions are those of the
runner of plan 085, called as that runner defines them.
"""

from __future__ import annotations

import contextlib
import io
import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from runner import __main__ as runner_main
from runner import sessions

import keycoverage.__main__ as coverage
from keycoverage.__main__ import Binding

CONFIGURATION = """\
(use-package alpha
  :bind (("C-c x" . alpha-run)
         :map alpha-mode-map
         ("q" . alpha-quit)))
(use-package beta
  :bind-keymap ("s-b" . beta-prefix-map))
(keymap-global-set "C-c y" #'top-level-command)
(global-set-key [remap undo] #'gamma-undo)
(keymap-global-set "C-c x" #'alpha-other)
(keymap-global-set "C-c z" #'removed-later)
(define-key global-map (kbd "C-c z") nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "b") #'dired-up-directory))
(define-prefix-command 'delta-map)
(define-key delta-map (kbd "d") #'delta-run)
(with-eval-after-load 'gamma
  (keymap-set gamma-mode-map "C-c d" 'delta-map))
(define-key key-translation-map (kbd "<menu>") 'event-apply-super-modifier)
"""

RUN = Binding("global", "C-c x", "alpha-run", "alpha", prefix=False)
UNDO = Binding("global", "<remap> <undo>", "gamma-undo", "(global)", prefix=False)
PREFIX = Binding("global", "s-b", "beta-prefix-map", "beta", prefix=True)
LAMBDA = Binding("global", "C-c l", ":lambda", "(global)", prefix=False)
DELTA = Binding("gamma-mode-map", "C-c d", "delta-map", "(gamma-mode-map)", prefix=True)
DELTA_RUN = Binding("delta-map", "d", "delta-run", "(delta-map)", prefix=False)


def spec_line(status: str, commands: list[object]) -> str:
    """Return a spec result line as tests/helpers/test-session.el writes it."""
    return json.dumps({"type": "spec", "file": "f.el", "name": f"S {status}",
                       "status": status, "disabled": False, "failure": None,
                       "commands": commands, "skip_entries": []})


class BindingTableTest(unittest.TestCase):
    """bindings.el and parse_bindings give the live keymap's bindings."""

    def test_the_last_binding_of_a_key_wins_and_removals_drop(self) -> None:
        """C-c x is rebound at top level, C-c z removed, the rest kept.

        A binding made outside use-package is grouped under its keymap.
        A key bound to a keymap with keymap-set is a prefix, as a
        :bind-keymap entry is.  A translation keymap runs no command, so
        its rows are left out.
        """
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "early-init.el").write_text(";; nothing\n")
            (root / "init.el").write_text(CONFIGURATION)
            exported = subprocess.run(
                [sessions.EMACS, "--batch", "-Q", "-l", str(coverage.EXPORTER),
                 str(root)],
                capture_output=True, text=True, timeout=60, check=True)
        self.assertCountEqual(coverage.parse_bindings(exported.stdout), [
            Binding("global", "C-c x", "alpha-other", "(global)", prefix=False),
            Binding("alpha-mode-map", "q", "alpha-quit", "alpha", prefix=False),
            Binding("global", "s-b", "beta-prefix-map", "beta", prefix=True),
            Binding("global", "C-c y", "top-level-command", "(global)", prefix=False),
            UNDO,
            Binding("dired-mode-map", "b", "dired-up-directory", "(dired-mode-map)",
                    prefix=False),
            DELTA_RUN,
            DELTA,
        ])

    def test_a_malformed_table_is_rejected(self) -> None:
        """Rows of another shape are an error, not an empty table."""
        with self.assertRaises(TypeError):
            coverage.parse_bindings('{"map": "global"}')
        with self.assertRaises(TypeError):
            coverage.parse_bindings('[{"map": "global", "key": "C-c x"}]')


class ExercisedTest(unittest.TestCase):
    """Which presses exercise a binding."""

    def test_its_command_from_its_key(self) -> None:
        """The key ran the command, possibly after a prefix argument."""
        self.assertTrue(coverage.exercised(RUN, {("alpha-run", "C-c x")}, {}))
        self.assertTrue(coverage.exercised(RUN, {("alpha-run", "C-u C-c x")}, {}))
        self.assertTrue(coverage.exercised(RUN, {("alpha-run", "C-u 4 C-c x")}, {}))
        self.assertTrue(coverage.exercised(RUN, {("alpha-run", "M-4 C-c x")}, {}))

    def test_not_another_command_or_another_key(self) -> None:
        """A shadowed key, M-x, or another binding of the command do not count."""
        self.assertFalse(coverage.exercised(RUN, {("other-command", "C-c x")}, {}))
        self.assertFalse(coverage.exercised(RUN, {("alpha-run", "C-c C-c x")}, {}))
        self.assertFalse(coverage.exercised(RUN, {("alpha-run", "x")}, {}))
        self.assertFalse(coverage.exercised(RUN, set(), {}))

    def test_a_remap_from_any_key(self) -> None:
        """A <remap> binding counts when its command ran, whatever the key."""
        self.assertTrue(coverage.exercised(UNDO, {("gamma-undo", "C-/")}, {}))
        self.assertFalse(coverage.exercised(UNDO, {("undo", "C-/")}, {}))

    def test_a_prefix_map_by_its_prefix(self) -> None:
        """A :bind-keymap prefix counts when keys starting with it ran."""
        self.assertTrue(coverage.exercised(PREFIX, {("beta-find", "s-b f")}, {}))
        self.assertFalse(coverage.exercised(PREFIX, {("other", "C-s-b f")}, {}))
        self.assertFalse(coverage.exercised(PREFIX, {("beta-prefix-map", "s-b")}, {}))

    def test_a_key_of_a_prefix_map_after_its_prefix(self) -> None:
        """A key of a keymap bound to a prefix counts when run after that prefix."""
        prefixes = {"delta-map": {"C-c d"}}
        self.assertTrue(coverage.exercised(DELTA_RUN, {("delta-run", "C-c d d")},
                                           prefixes))
        self.assertFalse(coverage.exercised(DELTA_RUN, {("delta-run", "C-c e d")},
                                            prefixes))
        self.assertFalse(coverage.exercised(DELTA_RUN, {("other", "C-c d d")},
                                            prefixes))

    def test_an_anonymous_command_by_its_key(self) -> None:
        """A lambda is recorded without a name; its key is enough."""
        self.assertTrue(coverage.exercised(LAMBDA, {(None, "C-c l")}, {}))
        self.assertFalse(coverage.exercised(LAMBDA, {(None, "C-c m")}, {}))


class PressesTest(unittest.TestCase):
    """Which recorded commands count."""

    def test_only_the_passed_specs_count(self) -> None:
        """A failed spec's commands are left out, as are malformed entries."""
        lines = [
            spec_line("passed", [{"command": "alpha-run", "keys": "C-c x"},
                                 {"command": None, "keys": "C-c l"},
                                 {"command": "broken"}, "not an entry"]),
            spec_line("failed", [{"command": "alpha-quit", "keys": "q"}]),
            spec_line("pending", [{"command": "gamma-undo", "keys": "C-/"}]),
        ]
        self.assertEqual(coverage.presses(lines),
                         {("alpha-run", "C-c x"), (None, "C-c l")})


class ReportTest(unittest.TestCase):
    """The Org report."""

    def test_counts_per_package_and_the_unexercised_list(self) -> None:
        """Totals, a row per package, and only the unexercised bindings listed."""
        equals = Binding("global", "C-=", "beta-expand", "beta", prefix=False)
        text = coverage.report([RUN, UNDO, PREFIX, equals],
                               {("alpha-run", "C-c x"), ("beta-go", "s-b g")}, "T")
        self.assertIn("2 of 4 user-facing bindings exercised.", text)
        self.assertIn("| alpha | 1 | 1 |\n", text)
        self.assertIn("| beta | 1 | 2 |\n", text)
        self.assertIn("| (global) | 0 | 1 |\n", text)
        unexercised = text.split("** Not exercised")[1]
        self.assertEqual(
            [line for line in unexercised.splitlines() if line.startswith("| ")][1:],
            ["| (global) | global | =<remap> <undo>= | gamma-undo |",
             "| beta | global | ~C-=~ | beta-expand |"])

    def test_a_press_through_a_prefix_bound_with_keymap_set(self) -> None:
        """The prefix and the key of its keymap both count from one press."""
        text = coverage.report([DELTA, DELTA_RUN], {("delta-run", "C-c d d")}, "T")
        self.assertIn("2 of 2 user-facing bindings exercised.", text)


class RunnerContractTest(unittest.TestCase):
    """main runs the sessions through the runner of plan 085, as it defines them."""

    def test_the_presses_of_the_sessions_reach_the_report(self) -> None:
        """A press recorded by a logic or a scenario session counts in the report.

        The filter selects both kinds of session.  The session functions
        are mocks specced from the real ones, so a call with the wrong
        arguments fails, and no Emacs starts.
        """
        bindings = coverage.parse_bindings(subprocess.run(
            [sessions.EMACS, "--batch", "-Q", "-l", str(coverage.EXPORTER),
             str(sessions.SOURCE)],
            capture_output=True, text=True, timeout=60, check=True).stdout)
        # Two bindings that no other row shares a key and a command with.
        logic_press, scenario_press = [
            b for b in bindings if not b.prefix and not b.key.startswith("<remap>")
            and [(c.key, c.command) for c in bindings].count((b.key, b.command)) == 1
        ][:2]

        def outcome(binding: Binding, directory: Path) -> sessions.Outcome:
            line = spec_line("passed", [{"command": binding.command,
                                         "keys": binding.key}])
            return sessions.Outcome(["f.el"], [line], None, "", None, directory, 0.0)

        def execute(jobs: list[runner_main.Job], _reaper: sessions.Reaper,
                    ) -> list[tuple[runner_main.Job, mock.Mock]]:
            for job in jobs:
                job.run()
            return [(job, mock.Mock(problems=[])) for job in jobs]

        output = io.StringIO()
        selected = runner_main.plan("fast", "journey", gui=False)
        with tempfile.TemporaryDirectory() as temporary, \
                mock.patch.object(
                    sessions, "run_logic", autospec=True,
                    side_effect=lambda _p, d, *_, **__: outcome(logic_press, d),
                ) as logic, \
                mock.patch.object(
                    sessions, "run_scenario", autospec=True,
                    side_effect=lambda _s, d, *_, **__: outcome(scenario_press, d),
                ) as scenario, \
                mock.patch.object(runner_main, "execute", autospec=True,
                                  side_effect=execute), \
                mock.patch.object(sessions, "choose_display", return_value=False), \
                mock.patch.object(coverage.tempfile, "mkdtemp",
                                  return_value=str(Path(temporary) / "root")), \
                contextlib.redirect_stdout(output):
            (Path(temporary) / "root").mkdir()
            status = coverage.main(["fast", "journey"])
            self.assertFalse((Path(temporary) / "root").exists())
        self.assertEqual(status, 0)
        self.assertEqual((logic.call_count, len(selected.logic)), (1, 1))
        self.assertEqual(scenario.call_count, len(selected.scenarios))
        self.assertGreater(scenario.call_count, 0)
        self.assertIn(f"2 of {len(bindings)} user-facing bindings exercised.",
                      output.getvalue())

if __name__ == "__main__":
    unittest.main()
