"""Self-tests of the runner's decisions: headers, result lines, skip accounting.

The expectations come from the rules of plan 085: a skip passes only
when a matching entry's reason is true, a stale entry fails, a disabled
spec fails, and a session must stay on its private socket and its
own private display.
"""

from __future__ import annotations

import json
import unittest

from runner import accounting

SOCKET = "/run/emacs-test-x/00-a/sockets/test"
EDITOR = f"emacsclient --socket-name={SOCKET}"
DISPLAY = ":7"  # the session's private display
FILE = "/repo/tests/scenarios/l1-a.el"


def spec(name: str, status: str = "passed", *, disabled: bool = False,
         entries: tuple[str, ...] = ()) -> dict[str, object]:
    """Return a spec line as a session writes it."""
    return {"type": "spec", "file": FILE, "name": name, "status": status,
            "disabled": disabled, "failure": None if status == "passed" else "why",
            "commands": [], "skip_entries": list(entries)}


def entry(entry_id: str, *, holds: bool | None, error: str | None = None,
          ) -> dict[str, object]:
    """Return an evaluated expected-skip entry as a session writes it."""
    return {"id": entry_id, "regexp": "x", "reason": '(:missing-executable "jdtls")',
            "holds": holds, "error": error}


def done(**facts: object) -> dict[str, object]:
    """Return a done line whose isolation facts are private unless FACTS say."""
    line: dict[str, object] = {"type": "done", "server_socket": SOCKET,
                               "editor": EDITOR, "visual": EDITOR,
                               "emacs_socket_name": None, "display": DISPLAY,
                               "wayland_display": None, "frame_displays": [DISPLAY]}
    line.update(facts)
    return line


def judge(*lines: dict[str, object], entries: tuple[dict[str, object], ...] = (),
          files: tuple[str, ...] = (FILE,), display: str | None = DISPLAY,
          ) -> accounting.Verdict:
    """Judge a session of a run on DISPLAY that wrote LINES, ENTRIES and done."""
    text = [json.dumps(line) for line in lines]
    text.append(json.dumps({"type": "skip-entries", "entries": list(entries)}))
    if not any(line["type"] == "done" for line in lines):
        text.append(json.dumps(done()))
    records, problems = accounting.parse_report(text)
    if problems:
        raise AssertionError(problems)
    return accounting.judge(records, list(files), SOCKET, display)


class HeaderTest(unittest.TestCase):
    """The scenario headers a runner reads before starting a session."""

    def test_a_file_without_headers_needs_nothing_special(self) -> None:
        """A plain scenario is not L3, not graphic, and opens no file."""
        headers = accounting.parse_headers(";;; a.el --- A -*- lexical-binding: t -*-")
        self.assertEqual(headers, accounting.Headers())

    def test_every_header_is_read_from_the_leading_comments(self) -> None:
        """All three headers are read, among other leading comments."""
        headers = accounting.parse_headers(
            ";;; a.el --- A\n;; scenario-tier: l3\n\n;;; Commentary:\n"
            ";; scenario-display: graphic\n"
            ";; scenario-startup: first-file clojure-nav/src/acme/core.clj\n"
            "(require 'scenario)\n")
        self.assertEqual(headers, accounting.Headers(
            l3=True, graphic=True, first_file="clojure-nav/src/acme/core.clj"))

    def test_a_header_after_the_first_form_is_not_a_header(self) -> None:
        """Only the leading comment block declares headers."""
        headers = accounting.parse_headers("(require 'x)\n;; scenario-tier: l3\n")
        self.assertFalse(headers.l3)

    def test_invalid_headers_are_rejected(self) -> None:
        """Unknown, malformed, repeated and escaping headers raise ValueError."""
        for text in (";; scenario-tiers: l3", ";; scenario-tier: l2",
                     ";; scenario-tier: l3\n;; scenario-tier: l3",
                     ";; scenario-startup: first-file",
                     ";; scenario-startup: first-file ../../init.el",
                     ";; scenario-startup: first-file /etc/passwd"):
            with self.subTest(text=text), self.assertRaises(ValueError):
                accounting.parse_headers(text)


class ReportTest(unittest.TestCase):
    """Result lines are parsed strictly at the boundary."""

    def test_malformed_lines_are_problems_and_the_rest_is_kept(self) -> None:
        """Bad JSON, unknown types, missing keys and bad statuses are reported."""
        lines = [json.dumps(spec("a ok")), "{not json",
                 json.dumps({"type": "surprise"}),
                 json.dumps({"type": "load-error", "file": FILE}),
                 json.dumps(spec("a odd", "maybe")),
                 json.dumps({**spec("a odder"), "skip_entries": [{"id": 1}]}),
                 json.dumps({"type": "skip-entries", "entries": [{"id": "x.el:1"}]})]
        records, problems = accounting.parse_report(lines)
        self.assertEqual([s["name"] for s in records["spec"]], ["a ok"])
        self.assertEqual(len(problems), 6)


class SkipAccountingTest(unittest.TestCase):
    """A skip passes only when a matching entry's reason holds."""

    def test_a_green_session_has_no_problem(self) -> None:
        """Passed specs and a private session give a clean verdict."""
        verdict = judge(spec("a one"), spec("a two"))
        self.assertEqual((verdict.problems, verdict.passed), ([], 2))

    def test_a_failed_spec_fails(self) -> None:
        """A failed spec is a problem naming the spec."""
        verdict = judge(spec("a one", "failed"))
        self.assertEqual(len(verdict.problems), 1)
        self.assertIn("a one", verdict.problems[0])

    def test_an_unlisted_skip_fails(self) -> None:
        """A skip that no entry matches is unexpected."""
        verdict = judge(spec("a one", "pending"))
        self.assertEqual(len(verdict.problems), 1)
        self.assertIn("unexpected skip", verdict.problems[0])

    def test_a_skip_whose_reason_holds_passes(self) -> None:
        """A skip matched by an entry whose reason holds is expected."""
        verdict = judge(spec("a one", "pending", entries=("p.el:1",)),
                        entries=(entry("p.el:1", holds=True),))
        self.assertEqual(verdict.problems, [])
        self.assertEqual(len(verdict.expected), 1)
        self.assertIn("jdtls", verdict.expected[0])

    def test_a_skip_whose_executable_exists_fails(self) -> None:
        """A skip matched only by an entry whose reason is false is unexpected."""
        verdict = judge(spec("a one", "pending", entries=("p.el:1",)),
                        entries=(entry("p.el:1", holds=False),))
        self.assertEqual(len(verdict.problems), 1)
        self.assertIn("unexpected skip", verdict.problems[0])

    def test_a_disabled_spec_fails_even_when_an_entry_holds(self) -> None:
        """An xit or xdescribe spec is never an expected skip."""
        verdict = judge(spec("a one", "pending", disabled=True, entries=("p.el:1",)),
                        spec("a two", "pending", entries=("p.el:1",)),
                        entries=(entry("p.el:1", holds=True),))
        self.assertEqual(len(verdict.problems), 1)
        self.assertIn("disabled", verdict.problems[0])

    def test_an_entry_whose_specs_ran_is_stale(self) -> None:
        """An entry that holds while its specs ran without skipping is stale."""
        verdict = judge(spec("a one", entries=("p.el:1",)),
                        entries=(entry("p.el:1", holds=True),))
        self.assertEqual(len(verdict.problems), 1)
        self.assertIn("stale", verdict.problems[0])

    def test_an_entry_whose_reason_is_false_is_not_stale(self) -> None:
        """An entry for another machine does not fail where its specs pass."""
        verdict = judge(spec("a one", entries=("p.el:1",)),
                        entries=(entry("p.el:1", holds=False),))
        self.assertEqual(verdict.problems, [])

    def test_an_entry_for_another_session_is_not_stale_here(self) -> None:
        """A session only judges the entries its own specs match."""
        verdict = judge(spec("a one"), entries=(entry("p.el:1", holds=True),))
        self.assertEqual(verdict.problems, [])


class SessionProblemTest(unittest.TestCase):
    """Load errors, empty files and lost sessions fail."""

    def test_a_load_error_fails_without_a_second_complaint(self) -> None:
        """A file that failed to load is reported once."""
        verdict = judge({"type": "load-error", "file": FILE, "error": "boom"})
        self.assertEqual(len(verdict.problems), 1)
        self.assertIn("boom", verdict.problems[0])

    def test_a_file_without_specs_fails(self) -> None:
        """A loaded file that defines no spec cannot fail, so it is a problem."""
        verdict = judge(spec("a one"), files=(FILE, "/repo/tests/b-test.el"))
        self.assertEqual(verdict.problems, ["/repo/tests/b-test.el: defines no specs"])

    def test_a_session_that_never_finished_fails(self) -> None:
        """Without a done line the specs may not all have run."""
        records, _ = accounting.parse_report([json.dumps(spec("a one"))])
        verdict = accounting.judge(records, [FILE], SOCKET, DISPLAY)
        self.assertEqual(len(verdict.problems), 1)


class IsolationTest(unittest.TestCase):
    """A session never reaches the user's Emacs."""

    def test_a_session_without_a_server_is_isolated(self) -> None:
        """A batch session runs no server at all."""
        verdict = judge(spec("a one"), done(server_socket=None))
        self.assertEqual(verdict.problems, [])

    def test_a_server_on_another_socket_fails(self) -> None:
        """Listening on the user's default socket is a problem."""
        user_socket = "/run/user/1000/emacs/server"
        verdict = judge(spec("a one"), done(server_socket=user_socket))
        self.assertEqual(len(verdict.problems), 1)

    def test_an_editor_on_another_socket_fails(self) -> None:
        """EDITOR and VISUAL must reach the session's own server."""
        verdict = judge(spec("a one"), done(editor="emacsclient", visual=None))
        self.assertEqual(len(verdict.problems), 2)

    def test_an_inherited_socket_name_fails(self) -> None:
        """EMACS_SOCKET_NAME would point emacsclient elsewhere."""
        verdict = judge(spec("a one"), done(emacs_socket_name="server"))
        self.assertEqual(len(verdict.problems), 1)


class DisplayIsolationTest(unittest.TestCase):
    """A session never draws on, or hands its children, the user's display."""

    def test_a_frame_on_the_user_display_fails(self) -> None:
        """One frame on another display is enough to fail the session."""
        verdict = judge(spec("a one"), done(frame_displays=[DISPLAY, ":0"]))
        self.assertEqual(len(verdict.problems), 1)
        self.assertIn("':0'", verdict.problems[0])

    def test_an_inherited_display_fails(self) -> None:
        """Subprocesses of the session would reach the user's display."""
        verdict = judge(spec("a one"), done(display=":0"))
        self.assertEqual(len(verdict.problems), 1)

    def test_an_inherited_wayland_display_fails(self) -> None:
        """A pgtk frame or a subprocess could reach the user's compositor."""
        verdict = judge(spec("a one"), done(wayland_display="wayland-0"))
        self.assertEqual(len(verdict.problems), 1)

    def test_a_run_without_a_display_allows_no_display_at_all(self) -> None:
        """With EMACS_TEST_GUI=0, any DISPLAY or graphical frame is a problem."""
        clean = judge(spec("a one"), done(display=None, frame_displays=[]),
                      display=None)
        self.assertEqual(clean.problems, [])
        leaking = judge(spec("a one"), done(display=":0", frame_displays=[":0"]),
                        display=None)
        self.assertEqual(len(leaking.problems), 2)


class RegistryTest(unittest.TestCase):
    """Entry problems across the sessions of a run."""

    def test_an_unusable_entry_is_reported_once(self) -> None:
        """Every session reports the same broken entry; the run says it once."""
        broken = (entry("p.el:2", holds=None, error="Unknown reason"),)
        verdicts = [judge(spec("a one"), entries=broken),
                    judge(spec("a two"), entries=broken)]
        self.assertEqual(accounting.registry_problems(verdicts, complete=False),
                         ["expected-skip entry p.el:2: Unknown reason"])

    def test_an_entry_matching_nothing_is_stale_only_in_a_complete_run(self) -> None:
        """A filtered run cannot know whether the entry's spec exists."""
        verdicts = [judge(spec("a one"), entries=(entry("p.el:1", holds=True),))]
        self.assertEqual(accounting.registry_problems(verdicts, complete=False), [])
        self.assertEqual(len(accounting.registry_problems(verdicts, complete=True)), 1)


if __name__ == "__main__":
    unittest.main()
