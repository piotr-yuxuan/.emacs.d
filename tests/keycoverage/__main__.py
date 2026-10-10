"""Report which key bindings of the configuration the tests exercise.

Usage: tests/coverage [fast|full [FILTER]]

The sessions are those of `tests/run PROFILE FILTER` (logic tests and
scenario files, see tests/runner/__main__.py), started by the same
runner of plan 085 on private displays.  Its command recorder lists,
per spec, each command run and the keys that ran it.  The bindings are
the binding table of plan 086 (tests/integrity/config-forms.el), read
from init.el and early-init.el by tests/keycoverage/bindings.el: one
row per map and key, the last binding winning as in the live keymap,
without the rows that remove a binding, and without the rows of the
translation keymaps, whose keys run no command.

A binding is exercised when a spec that passed ran its command from its
key, possibly after a prefix argument (C-u, M-4); a <remap> binding,
when its command ran from any key; a prefix, when keys starting with it
ran a command; a binding whose command the readers do not name (a
lambda), when its key ran anything.  A prefix binds a key to a keymap:
a :bind-keymap entry, or a command named, as Emacs names keymaps, with
the suffix -map.  A key of a keymap bound to a prefix also runs after
that prefix, so it is exercised from either.  The recorder does not say
which keymap a key came from, so one press counts for every map that
binds the same key to the same command.

The report is two Org tables: exercised and total bindings per package
(the enclosing use-package; for a binding made outside one, its keymap
in parentheses), then the bindings not exercised.  It is
informational: the exit status is 0 whenever the report is printed,
also when sessions failed (their failed specs do not count), and 2
when the report cannot be made.  Everything is written in a temporary
directory, removed at the end unless a session failed.
"""

from __future__ import annotations

import argparse
import dataclasses
import functools
import json
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING

from runner import __main__ as runner_main
from runner import accounting, sessions

if TYPE_CHECKING:
    from collections.abc import Callable, Collection, Mapping

EXPORTER = Path(__file__).resolve().parent / "bindings.el"
EXPORT_SECONDS = 60
ANONYMOUS = frozenset({":lambda", ":unresolved"})  # commands the readers do not name
# The keymaps that translate events (elisp manual, Translation Keymaps).
TRANSLATION_MAPS = frozenset({"input-decode-map", "local-function-key-map",
                              "function-key-map", "key-translation-map"})
ROW_SCHEMA = {"map": str, "key": str, "command": (str, type(None)),
              "package": (str, type(None)), "prefix": bool}
# Leading prefix-argument keys, each followed by a space: C-u, then
# digits or a minus sign, or the digit and minus keys with modifiers.
PREFIX_ARGUMENT = re.compile(r"(?:(?:C-u|(?:C-|M-|C-M-)?[0-9-]) )*")

Press = tuple[str | None, str]  # (command, keys), as the recorder wrote them


@dataclasses.dataclass(frozen=True)
class Binding:
    """One user-facing row of the binding table."""

    map: str
    key: str
    command: str  # a symbol name, or one of ANONYMOUS
    package: str  # the enclosing use-package, or "(MAP)" outside one
    prefix: bool  # a prefix: its command is a keymap


def parse_bindings(text: str) -> list[Binding]:
    """Return the user-facing bindings of TEXT, the JSON of bindings.el.

    Keep one row per map and key, the last one, as the live keymap
    does; a last row whose command is null removes the key.  Leave out
    the translation keymaps.  Raise ValueError when TEXT is not JSON,
    and TypeError when it is not an array of the exporter's rows.
    """
    rows = json.loads(text)
    if not isinstance(rows, list):
        message = "the binding table is not a JSON array"
        raise TypeError(message)
    table: dict[tuple[str, str], Binding | None] = {}
    for row in rows:
        accounting.conform(row, ROW_SCHEMA)
        command = row["command"]
        table[row["map"], row["key"]] = command and Binding(
            row["map"], row["key"], command, row["package"] or f"({row['map']})",
            row["prefix"] or command.endswith("-map"))
    return [binding for binding in table.values()
            if binding and binding.map not in TRANSLATION_MAPS]


def presses(lines: list[str]) -> set[Press]:
    """Return the (command, keys) run by the passed specs of a session.

    LINES are the session's result lines.  A recorder entry that is not
    {"command": NAME or null, "keys": KEYS} is ignored.
    """
    records, _ = accounting.parse_report(lines)
    entries = [entry for spec in records["spec"] if spec["status"] == "passed"
               for entry in spec["commands"] if isinstance(entry, dict)]
    return {(entry.get("command"), entry.get("keys")) for entry in entries
            if isinstance(entry.get("command"), (str, type(None)))
            and isinstance(entry.get("keys"), str)}


def exercised(binding: Binding, pressed: Collection[Press],
              prefixes: Mapping[str, Collection[str]]) -> bool:
    """Return true when PRESSED, the presses of passed specs, exercise BINDING.

    PREFIXES maps a keymap to the keys of the prefixes bound to it.
    """
    if binding.prefix:
        return any(keys.startswith(f"{binding.key} ") for _, keys in pressed)
    if binding.key.startswith("<remap> "):
        return any(command == binding.command for command, _ in pressed)
    anonymous = binding.command in ANONYMOUS
    reached = {binding.key, *(f"{prefix} {binding.key}"
                              for prefix in prefixes.get(binding.map, ()))}
    return any((anonymous or command == binding.command)
               and keys[PREFIX_ARGUMENT.match(keys).end():] in reached
               for command, keys in pressed)


def report(bindings: list[Binding], pressed: Collection[Press], title: str) -> str:
    """Return the Org report of which BINDINGS the presses PRESSED exercise."""
    prefixes: dict[str, set[str]] = {}
    for binding in bindings:
        if binding.prefix:
            prefixes.setdefault(binding.command, set()).add(binding.key)
    done = {binding for binding in bindings if exercised(binding, pressed, prefixes)}
    lines = [f"* Key binding coverage: {title}", "",
             f"{len(done)} of {len(bindings)} user-facing bindings exercised.", "",
             "| Package | Exercised | Bindings |", "|-"]
    for package in sorted({binding.package for binding in bindings}):
        rows = [binding for binding in bindings if binding.package == package]
        lines.append(f"| {package} | {len(done.intersection(rows))} | {len(rows)} |")
    lines += [f"| total | {len(done)} | {len(bindings)} |", "",
              "** Not exercised", "", "| Package | Map | Key | Command |", "|-"]
    for b in sorted(set(bindings) - done, key=lambda b: (b.package, b.map, b.key)):
        verbatim = "~" if "=" in b.key else "="  # as in docs/reference/keys.org
        lines.append(f"| {b.package} | {b.map} | {verbatim}{b.key}{verbatim}"
                     f" | {b.command} |")
    return "\n".join(lines) + "\n"


def main(arguments: list[str]) -> int:
    """Run the sessions ARGUMENTS name, print the report; return the exit status."""
    parser = argparse.ArgumentParser(
        prog="tests/coverage", description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("profile", nargs="?", choices=("fast", "full"), default="fast")
    parser.add_argument("filter", nargs="?", default="")
    options = parser.parse_args(arguments)
    try:
        graphic = sessions.choose_display(os.environ.get("EMACS_TEST_GUI"),
                                          sessions.emacs_features(), sessions.XVFB)
        exporter = [sessions.EMACS, "--batch", "-Q", "-l", str(EXPORTER),
                    str(sessions.SOURCE)]
        bindings = parse_bindings(subprocess.run(
            exporter, capture_output=True, text=True, timeout=EXPORT_SECONDS,
            check=True).stdout)
    except (sessions.UsageError, subprocess.SubprocessError, OSError, ValueError,
            TypeError) as error:
        sys.stderr.write(f"tests/coverage: {error}.\n")
        return 2
    selected = runner_main.plan(options.profile, options.filter, gui=graphic)
    if not (selected.logic or selected.scenarios):
        sys.stderr.write(f"tests/coverage: no session of {options.profile} matches"
                         f" {options.filter!r}.\n")
        return 2
    root = Path(tempfile.mkdtemp(prefix="emacs-cov-"))
    reaper = sessions.Reaper()
    outcomes: list[sessions.Outcome] = []

    def kept(session: Callable[[], sessions.Outcome]) -> sessions.Outcome:
        outcome = session()
        outcomes.append(outcome)
        return outcome

    logic = functools.partial(sessions.run_logic, selected.logic, root / "logic",
                              reaper, seconds=runner_main.LOGIC_SECONDS)
    jobs = ([runner_main.Job("logic and self-tests", functools.partial(kept, logic))]
            if selected.logic else [])
    jobs += [runner_main.Job(runner_main.name(scenario.path), functools.partial(
        kept, functools.partial(sessions.run_scenario, scenario, root / f"{index:02d}",
                                reaper, graphic=graphic)), narrow=scenario.headers.l3)
             for index, scenario in enumerate(selected.scenarios)]
    try:
        results = runner_main.execute(jobs, reaper)
    except KeyboardInterrupt:
        sys.stderr.write(f"Interrupted; session directories kept in {root}\n")
        return 130
    failing = sum(bool(verdict.problems) for _, verdict in results)
    title = f"tests/run {options.profile} {options.filter}".strip()
    sys.stdout.write("\n" + report(bindings, set().union(*map(presses, (
        outcome.lines for outcome in outcomes))), title))
    sys.stdout.write(f"\n{len(results)} sessions, {failing} failing.\n")
    if failing:
        sys.stdout.write("Failed specs do not count; session directories kept in"
                         f" {root}\n")
    else:
        shutil.rmtree(root, ignore_errors=True)
    return 0


if __name__ == "__main__":
    # Stop the sessions cleanly on Ctrl-C, kill and hangup, as tests/run does.
    signal.signal(signal.SIGTERM, signal.default_int_handler)
    signal.signal(signal.SIGHUP, signal.default_int_handler)
    sys.exit(main(sys.argv[1:]))
