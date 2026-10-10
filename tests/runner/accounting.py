"""Decide what a test session means from what it reported.  No I/O.

A session writes JSON lines (see tests/helpers/test-session.el).  They are
parsed here at the boundary, then judged:
- a failed spec, a disabled spec (xit, xdescribe), a load error, an
  unreadable line, or a session that never finished is a problem;
- a skipped spec is expected only when an expected-skip entry matches
  it and the entry's reason holds in that session;
- an entry whose reason holds is stale when specs it matches ran and
  none of them skipped;
- the session's server socket and EDITOR must be its private ones, and
  its only display its own private one, or none.
"""

from __future__ import annotations

import dataclasses
import json
import re
from pathlib import PurePosixPath
from typing import Any

Record = dict[str, Any]

HEADER = re.compile(r";+\s*scenario-([a-z]+):\s*(.*?)\s*")
HEADER_VALUES = {
    "tier": re.compile(r"l3"),
    "display": re.compile(r"graphic"),
    "startup": re.compile(r"first-file (\S+)"),
}

STATUSES = frozenset({"passed", "failed", "pending"})
OPTIONAL_TEXT = (str, type(None))
SCHEMAS = {
    "spec": {
        "file": OPTIONAL_TEXT,
        "name": str,
        "status": str,
        "disabled": bool,
        "failure": OPTIONAL_TEXT,
        "commands": list,
        "skip_entries": list,
    },
    "load-error": {"file": str, "error": str},
    "skip-entries": {"entries": list},
    "done": {
        "server_socket": OPTIONAL_TEXT,
        "editor": OPTIONAL_TEXT,
        "visual": OPTIONAL_TEXT,
        "emacs_socket_name": OPTIONAL_TEXT,
        "display": OPTIONAL_TEXT,
        "wayland_display": OPTIONAL_TEXT,
        "frame_displays": list,
    },
}
ENTRY_SCHEMA = {
    "id": str,
    "regexp": OPTIONAL_TEXT,
    "reason": str,
    "holds": (bool, type(None)),
    "error": OPTIONAL_TEXT,
}


@dataclasses.dataclass(frozen=True)
class Headers:
    """What a scenario file declares about the session it needs."""

    l3: bool = False
    graphic: bool = False
    first_file: str | None = None  # relative to tests/fixtures/


def parse_headers(text: str) -> Headers:
    """Return the Headers declared in the leading comment block of TEXT.

    Raise ValueError for an unknown, repeated or malformed header.
    """
    values = {}
    for line in text.splitlines():
        stripped = line.strip()
        if stripped and not stripped.startswith(";"):
            break
        match = HEADER.fullmatch(stripped)
        if not match:
            continue
        key, value = match.groups()
        pattern = HEADER_VALUES.get(key)
        valid = pattern and pattern.fullmatch(value)
        if key in values or not valid:
            message = f"unknown, repeated or malformed header: {stripped}"
            raise ValueError(message)
        values[key] = valid
    fixture = values["startup"].group(1) if "startup" in values else None
    if fixture and (fixture.startswith("/") or ".." in PurePosixPath(fixture).parts):
        message = f"the first-file fixture leaves tests/fixtures/: {fixture}"
        raise ValueError(message)
    return Headers(l3="tier" in values, graphic="display" in values, first_file=fixture)


def conform(record: Record, schema: dict[str, type | tuple[type, ...]]) -> None:
    """Raise TypeError unless RECORD has every key of SCHEMA with its type."""
    for key, types in schema.items():
        if key not in record or not isinstance(record[key], types):
            message = f"{key!r} is missing or of the wrong type"
            raise TypeError(message)


def parse_report(lines: list[str]) -> tuple[dict[str, list[Record]], list[str]]:
    """Parse a session's result LINES into records grouped by type.

    Return (records, problems): records maps each type of SCHEMAS to a
    list of validated dictionaries; problems describes rejected lines.
    """
    records: dict[str, list[Record]] = {kind: [] for kind in SCHEMAS}
    problems = []
    for number, line in enumerate(lines, 1):
        try:
            record = json.loads(line)
            conform(record, SCHEMAS[record["type"]])
            for entry in record.get("entries", []):
                conform(entry, ENTRY_SCHEMA)
        except (ValueError, KeyError, TypeError) as error:
            problems.append(f"result line {number} is malformed ({error})")
            continue
        if record["type"] == "spec" and (
                record["status"] not in STATUSES
                or not all(isinstance(i, str) for i in record["skip_entries"])):
            problems.append(f"result line {number} has a bad status or entry id")
            continue
        records[record["type"]].append(record)
    return records, problems


def skipped(spec: Record) -> bool:
    """Return true for a spec that skipped at run time, as opposed to disabled."""
    return spec["status"] == "pending" and not spec["disabled"]


@dataclasses.dataclass
class Verdict:
    """The judgement of one session."""

    problems: list[str]
    passed: int
    expected: list[str]  # the expected skips, with the reasons that allow them
    matched: set[str]  # ids of the expected-skip entries some spec matched
    entries: dict[str, Record]  # id -> entry, as the session evaluated it
    statuses: dict[str, str]  # spec name -> status


def judge(records: dict[str, list[Record]], files: list[str], socket: str,
          display: str | None) -> Verdict:
    """Judge one session's RECORDS.

    FILES are the test files the session loaded, named as the session
    names them; SOCKET is the session's private server socket; DISPLAY
    is the session's private display, or None when it has none.
    """
    load_errors = records["load-error"]
    problems = [f"{r['file']}: failed to load: {r['error']}" for r in load_errors]
    entries = {e["id"]: e for r in records["skip-entries"] for e in r["entries"]}
    holding = {i for i, e in entries.items() if e["holds"]}
    specs = records["spec"]
    expected = []
    for spec in specs:
        allowing = sorted(holding.intersection(spec["skip_entries"]))
        if spec["status"] == "failed":
            problems.append(f"{spec['name']}: {spec['failure']}")
        elif spec["disabled"]:
            problems.append(f"{spec['name']}: disabled with xit or xdescribe")
        elif skipped(spec) and not allowing:
            problems.append(f"{spec['name']}: unexpected skip: {spec['failure']}")
        elif skipped(spec):
            reasons = ", ".join(entries[i]["reason"] for i in allowing)
            expected.append(f"{spec['name']}: {reasons}")
    for entry_id in sorted(holding):
        matches = [s for s in specs if entry_id in s["skip_entries"]]
        if matches and not any(skipped(s) for s in matches):
            problems.append(f"expected-skip entry {entry_id} is stale: its specs ran")
    reported = {s["file"] for s in specs} | {r["file"] for r in load_errors}
    problems += [f"{f}: defines no specs" for f in files if f not in reported]
    problems += isolation_problems(records["done"], socket, display)
    passed = sum(s["status"] == "passed" for s in specs)
    matched = {i for s in specs for i in s["skip_entries"]}
    statuses = {s["name"]: s["status"] for s in specs}
    return Verdict(problems, passed, expected, matched, entries, statuses)


def registry_problems(verdicts: list[Verdict], *, complete: bool) -> list[str]:
    """Return the problems of the expected-skip entries across a run.

    An entry that cannot be read or evaluated is a problem.  When the
    run is COMPLETE, every test file ran, so an entry that matched no
    spec at all is stale too.
    """
    entries = {i: e for v in verdicts for i, e in v.entries.items()}
    matched = set().union(*(v.matched for v in verdicts))
    problems = [f"expected-skip entry {i}: {e['error']}"
                for i, e in sorted(entries.items()) if e["error"]]
    if complete:
        problems += [f"expected-skip entry {i} is stale: it matches no spec"
                     for i in sorted(entries.keys() - matched)
                     if not entries[i]["error"]]
    return problems


def isolation_problems(done: list[Record], socket: str,
                       display: str | None) -> list[str]:
    """Return why the session's last DONE record is missing or not isolated.

    A session may run no server at all; if it runs one, it listens on
    SOCKET.  EDITOR and VISUAL name SOCKET; EMACS_SOCKET_NAME is unset.
    DISPLAY, the session's private display or None, is its only
    display: its DISPLAY variable and that of every graphical frame;
    WAYLAND_DISPLAY is unset.  Kept apart from `judge' to hold it under
    the complexity limit.
    """
    if not done:
        return ["the session never reported that it finished"]
    facts = done[-1]
    problems = []
    if facts["server_socket"] not in {None, socket}:
        problems.append(f"its server listens on {facts['server_socket']}")
    problems.extend(
        f"{name.upper()} does not name the private socket: {facts[name]}"
        for name in ("editor", "visual")
        if socket not in (facts[name] or "")
    )
    if facts["emacs_socket_name"]:
        problems.append(f"EMACS_SOCKET_NAME is set: {facts['emacs_socket_name']}")
    if facts["display"] != display:
        problems.append(f"DISPLAY is {facts['display']}, not the session's {display}")
    if facts["wayland_display"]:
        problems.append(f"WAYLAND_DISPLAY is set: {facts['wayland_display']}")
    strays = [shown for shown in facts["frame_displays"] if shown != display]
    if strays:
        problems.append(f"frames on displays other than its own {display}: {strays}")
    return problems
