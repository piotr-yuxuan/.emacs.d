"""Run the T1 integrity checks of plan 086, or regenerate the key cheatsheet.

Usage: tests/integrity/session/run [check|cheatsheet]

check       run t1.el and t1-gui.el, the T1 checks.  This is the default.
cheatsheet  run cheatsheet.el, which writes docs/reference/keys.org.

Each file runs in a fresh daemon session started by the runner of plan
085 (tests/runner/), exactly as it starts a scenario file, on the
runner's private display (EMACS_TEST_GUI is honoured the same way, and
a file declaring ';; scenario-display: graphic' is skipped without a
display), except that the session's init directory also holds the files
of tests/integrity/fixtures/session/local/: a fixture local.el, which
the configuration loads at the end of its startup as it loads the
user's own, and the Org file it names.

Loading every configured package loads magit, which asks git for its
version inside its repository in the live straight tree, and `git
describe --dirty` may rewrite that repository's index.  085's runner
gives every session each repository of $EMACS_LIVE_DIR/straight/repos as
a GIT_CEILING_DIRECTORIES entry, where git finds no repository, and this
run fails when the .git directory of one of them has changed.

The exit status is 1 on any failure and 2 on a usage error.  The
directory of a green session is removed; otherwise it is kept and printed.
"""

from __future__ import annotations

import functools
import os
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path

from runner import accounting, sessions
from runner.__main__ import Job, execute

HERE = Path(__file__).resolve().parent
LOCAL = sessions.TESTS / "integrity" / "fixtures" / "session" / "local"
FILES = {"check": [HERE / "t1.el", HERE / "t1-gui.el"],
         "cheatsheet": [HERE / "cheatsheet.el"]}


def main(arguments: list[str], *, gui: bool) -> int:
    """Run the sessions named by ARGUMENTS; return the exit status.

    GUI says whether the sessions may create a graphical frame.
    """
    kind = arguments[0] if arguments else "check"
    if kind not in FILES or len(arguments) > 1:
        sys.stderr.write(__doc__)
        return 2
    root = Path(tempfile.mkdtemp(prefix="emacs-t1-"))
    reaper = sessions.Reaper()
    jobs = []
    for index, path in enumerate(FILES[kind]):
        headers = accounting.parse_headers(path.read_text(encoding="utf-8"))
        if headers.graphic and not gui:
            sys.stdout.write(f"skip  integrity {path.name}: needs a graphical display"
                             " (EMACS_TEST_GUI=0)\n")
            continue
        directory = root / f"{index:02d}"
        shutil.copytree(LOCAL, directory / "init")
        jobs.append(Job(f"integrity {path.name}", functools.partial(
            sessions.run_scenario, sessions.Scenario(path, headers), directory, reaper,
            graphic=gui)))
    repositories = sorted((sessions.LIVE / "straight" / "repos").glob("*/.git"))
    before = {git: git.stat().st_mtime_ns for git in repositories}
    signal.signal(signal.SIGTERM, signal.default_int_handler)
    try:
        results = execute(jobs, reaper)
    except KeyboardInterrupt:
        sys.stderr.write(f"Interrupted; session directories kept in {root}\n")
        return 130
    written = [git.parent.name for git in repositories
               if git.stat().st_mtime_ns != before[git]]
    if written:
        sys.stderr.write(f"The sessions wrote to the live repositories {written}\n")
    if written or any(verdict.problems for _, verdict in results):
        return 1
    root.rmdir()
    return 0


if __name__ == "__main__":
    try:
        graphic = sessions.choose_display(os.environ.get("EMACS_TEST_GUI"),
                                          sessions.emacs_features(), sessions.XVFB)
    except (sessions.UsageError, subprocess.SubprocessError, OSError) as error:
        sys.stderr.write(f"tests/integrity/session/run: {error}.\n")
        sys.exit(2)
    sys.exit(main(sys.argv[1:], gui=graphic))
