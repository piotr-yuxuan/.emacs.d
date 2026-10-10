"""Start hermetic Emacs sessions for the tests and collect what they report.

Each session owns a fresh directory:
- init/       the --init-directory: an early-init.el and init.el that load
              the configuration under test, with empty var/ and etc/, no
              local.el, and its own native-code cache;
- sock/       the private server socket and the MCP socket;
- tmp/        TMPDIR, so temporary files die with the session;
- sandbox/    the copy of a first-file fixture;
- results.jsonl, emacs.log and messages.txt.

Packages, native code and tree-sitter grammars are read from the live
installation (EMACS_LIVE_DIR, ~/.emacs.d by default), with straight.el
in safe mode, so nothing there is written.  The real HOME is kept: L3
needs ~/.m2 and the login shell's PATH.  Git reads only
tests/runner/gitconfig, and no GIT_* variable of the caller.
"""

from __future__ import annotations

import contextlib
import dataclasses
import json
import os
import select
import shutil
import signal
import subprocess
import threading
import time
from pathlib import Path
from typing import TYPE_CHECKING

from runner import accounting

if TYPE_CHECKING:
    from collections.abc import Callable, Iterator

TESTS = Path(__file__).resolve().parents[1]
SOURCE = TESTS.parent
LIVE = Path(os.environ.get("EMACS_LIVE_DIR", "~/.emacs.d")).expanduser()
GITCONFIG = TESTS / "runner" / "gitconfig"
EMACS = shutil.which("emacs") or "emacs"
EMACSCLIENT = shutil.which("emacsclient") or "emacsclient"
GIT = shutil.which("git") or "git"
SH = shutil.which("sh") or "/bin/sh"
PS = shutil.which("ps") or "/bin/ps"
XVFB = shutil.which("Xvfb")
XVFB_SCREEN = "1920x1080x24"
XVFB_SECONDS = 10
X_FEATURES = frozenset({"X11", "PGTK"})  # Emacs builds that can draw on Xvfb
# sh -c WATCHDOG watchdog SERVER: kill SERVER once the shell's parent, the
# runner, has exited, which gives the shell another parent.  When ps
# fails, the parent is unknown, and the server stays.
WATCHDOG = ('until p=$(ps -o ppid= -p $$) && [ -n "$p" ] && [ "$p" -ne "$PPID" ];'
            ' do sleep 1; done; kill "$1"')
NOT_INHERITED = frozenset({"EMACS_SOCKET_NAME", "DISPLAY", "WAYLAND_DISPLAY"})
SERVER_NAME = "test"
# A Unix socket path holds 104 bytes on macOS and 108 on Linux; leave room
# for the longest socket name, emacs-mcp-server.sock.
SOCKET_DIRECTORY_LIMIT = 80
SCENARIO_SECONDS = 120
L3_SECONDS = 300
POLL_SECONDS = 0.1
STOP_SECONDS = 10

EARLY_INIT = """\
;;; early-init.el --- Hermetic test session -*- lexical-binding: t -*-
;; Written by tests/runner/sessions.py.
(setq straight-base-dir {live}
      straight-safe-mode t
      server-name {server_name}
      server-socket-dir {sockets})
(with-eval-after-load 'mcp-server
  (setq mcp-server-socket-directory {sockets}))
(setq native-comp-eln-load-path
      (append (list {own_eln} {live_eln}) native-comp-eln-load-path))
;; End the session once the runner that started it is gone, even when
;; the runner was killed without cleaning up: Emacs then has another
;; parent.  A daemon never exits on its own.
(run-with-timer
 1 1 (lambda ()
       (let ((parent (alist-get 'ppid (process-attributes (emacs-pid)))))
         (when (and parent (/= parent {runner}))
           (kill-emacs 1)))))
(load {session_el} nil t t)
(load {early_init} nil t t)
"""

INIT = """\
;;; init.el --- Hermetic test session -*- lexical-binding: t -*-
;; Written by tests/runner/sessions.py.  Startup code such as the
;; dashboard reads `command-line-args': show it those of a plain
;; `emacs --daemon', without the harness's --init-directory.
(let ((command-line-args
       (cons (car command-line-args) command-line-args-left)))
  (load {init} nil t t))
"""

SAVE_MESSAGES = """\
(with-current-buffer (messages-buffer)
  (write-region nil nil {path} nil 'silent))"""


class UsageError(Exception):
    """The run cannot start as asked; the message says what to change."""


def choose_display(switch: str | None, features: frozenset[str],
                   xvfb: str | None) -> bool:
    """Decide whether the sessions of a run get a private display.

    SWITCH is the value of EMACS_TEST_GUI: unset or empty by default,
    "0" for no graphical frame at all.  FEATURES are the words of the
    `system-configuration-features' of Emacs; XVFB is the Xvfb program,
    or None.  Test frames never go to the user's screen, and a run
    never drops the graphical scenarios silently: when no private
    display is possible and the switch is not "0", raise UsageError.
    """
    if switch == "0":
        return False
    if switch:
        message = f"EMACS_TEST_GUI must be unset or 0, not {switch!r}"
        raise UsageError(message)
    if not features & X_FEATURES:
        message = ("this Emacs cannot draw on a private X display: it is built"
                   " without X11 or PGTK, as on macOS.  Run with EMACS_TEST_GUI=0")
        raise UsageError(message)
    if xvfb is None:
        message = ("Xvfb is not installed, so test frames have no private display."
                   "  Install it, or run with EMACS_TEST_GUI=0")
        raise UsageError(message)
    return True


def emacs_features() -> frozenset[str]:
    """Return the words of the `system-configuration-features' of Emacs."""
    result = subprocess.run(
        [EMACS, "-Q", "--batch", "--eval", "(princ system-configuration-features)"],
        capture_output=True, text=True, timeout=STOP_SECONDS, check=True)
    return frozenset(result.stdout.split())


@contextlib.contextmanager
def private_display() -> Iterator[str]:
    """Start a private Xvfb server; yield its display name, such as ":3".

    It is the display of one session, so the session's frames never
    reach the user's screen, and X selections such as the clipboard,
    which belong to a display, are never shared with another session.
    On exit, stop the server.  A watchdog stops it as well when this
    process dies without cleaning up, as under SIGKILL.
    """
    reader, writer = os.pipe()
    server = subprocess.Popen(
        [XVFB, "-displayfd", str(writer), "-screen", "0", XVFB_SCREEN,
         "-nolisten", "tcp"],
        pass_fds=(writer,), stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL, start_new_session=True)
    os.close(writer)
    started = [server]  # stopped in order: the watchdog first, as it kills by PID
    try:
        started.insert(0, subprocess.Popen(
            [SH, "-c", WATCHDOG, "watchdog", str(server.pid)],
            stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL, start_new_session=True))
        ready, _, _ = select.select([reader], [], [], XVFB_SECONDS)
        number = os.read(reader, 16).decode().strip() if ready else ""
        if not number.isdigit():
            message = f"Xvfb gave no display within {XVFB_SECONDS} s"
            raise SessionError(message)
        yield f":{number}"
    finally:
        os.close(reader)
        for process in started:
            with contextlib.suppress(ProcessLookupError):
                os.killpg(process.pid, signal.SIGTERM)
            with contextlib.suppress(subprocess.TimeoutExpired):
                process.wait(timeout=STOP_SECONDS)
            process.kill()
            process.wait()


class SessionError(Exception):
    """A session could not start, stopped early or missed its deadline."""


class InterruptedRunError(Exception):
    """The run is stopping; no new session may start."""


def elisp_string(value: object) -> str:
    """Return VALUE as an Emacs Lisp string literal."""
    return json.dumps(str(value))


@dataclasses.dataclass(frozen=True)
class Session:
    """The files and environment of one prepared session."""

    directory: Path
    init: Path
    socket: Path
    results: Path
    log: Path
    environment: dict[str, str]


@dataclasses.dataclass(frozen=True)
class Scenario:
    """A scenario file to run, and the one spec to run alone, if any."""

    path: Path
    headers: accounting.Headers
    only: str | None = None  # the full name of the spec to run alone


@dataclasses.dataclass
class Outcome:
    """What one session produced, as the runner saw it."""

    files: list[str]  # the test files, as the session names them
    lines: list[str]  # the result lines
    error: str | None  # a problem the runner itself detected
    socket: str
    display: str | None  # the session's private display, or None
    directory: Path
    seconds: float


def prepare(directory: Path, display: str | None) -> Session:
    """Create a hermetic session in the new DIRECTORY and return it.

    DISPLAY is the session's private X display, such as ":3", or None for a
    session that has no display at all.  The caller's displays are
    never inherited.
    """
    init = directory / "init"
    sockets = directory / "sock"
    socket = sockets / SERVER_NAME
    if len(os.fsencode(sockets)) > SOCKET_DIRECTORY_LIMIT:
        message = f"socket directory too long for Unix sockets: {sockets}"
        raise SessionError(message)
    (init / "var").mkdir(parents=True)
    (init / "etc").mkdir()
    (directory / "tmp").mkdir()
    if (LIVE / "tree-sitter").is_dir():
        (init / "tree-sitter").symlink_to(LIVE / "tree-sitter")
    (init / "early-init.el").write_text(EARLY_INIT.format(
        live=elisp_string(LIVE),
        server_name=elisp_string(SERVER_NAME),
        sockets=elisp_string(sockets),
        own_eln=elisp_string(init / "eln-cache"),
        live_eln=elisp_string(LIVE / "eln-cache"),
        runner=os.getpid(),
        session_el=elisp_string(TESTS / "helpers" / "test-session.el"),
        early_init=elisp_string(SOURCE / "early-init.el"),
    ))
    (init / "init.el").write_text(INIT.format(init=elisp_string(SOURCE / "init.el")))
    # A git hook exports GIT_DIR, GIT_INDEX_FILE and more: a session's
    # git must see only its sandbox and the configuration set below.
    environment = {name: value for name, value in os.environ.items()
                   if not name.startswith("GIT_") and name not in NOT_INHERITED}
    if display:
        environment.update(DISPLAY=display, GDK_BACKEND="x11")
    editor = f"{EMACSCLIENT} --socket-name={socket}"
    environment.update(
        EDITOR=editor,
        VISUAL=editor,
        GIT_CONFIG_GLOBAL=str(GITCONFIG),
        GIT_CONFIG_NOSYSTEM="1",
        # Loading a package such as magit runs git in its straight.el
        # repository (`git describe --dirty' takes the index lock); git
        # must find no repository there, so the live tree stays unwritten.
        GIT_CEILING_DIRECTORIES=os.pathsep.join(
            str(path) for path in sorted((LIVE / "straight" / "repos").glob("*"))),
        TMPDIR=str(directory / "tmp"),
    )
    return Session(directory, init, socket, directory / "results.jsonl",
                   directory / "emacs.log", environment)


def process_rows() -> list[tuple[int, int, str]]:
    """Return (PID, parent PID, start time) for every process, from ps.

    Return no row when ps fails: stopping a session must go on.
    """
    try:
        listing = subprocess.run([PS, "-A", "-o", "pid=,ppid=,lstart="],
                                 capture_output=True, text=True,
                                 timeout=STOP_SECONDS, check=True).stdout
    except (OSError, subprocess.SubprocessError):
        return []
    rows = (line.split(None, 2) for line in listing.splitlines())
    return [(int(pid), int(parent), start) for pid, parent, start in rows]


def descendants(pid: int, rows: list[tuple[int, int, str]]) -> dict[int, str]:
    """Return every process below PID in ROWS, as {PID: start time}.

    ROWS hold (PID, parent PID, start time) for every process.  The
    start time tells a process from a later one that reuses its PID.
    """
    children: dict[int, list[tuple[int, str]]] = {}
    for child, parent, start in rows:
        children.setdefault(parent, []).append((child, start))
    found: dict[int, str] = {}
    frontier = [pid]
    while frontier:
        below = [(child, start) for parent in frontier
                 for child, start in children.get(parent, []) if child not in found]
        found.update(below)
        frontier = [child for child, _ in below]
    return found


class Reaper:
    """Own every Emacs this run starts, and the processes each one starts."""

    def __init__(self) -> None:
        """Start with no process and accepting new ones."""
        self._lock = threading.Lock()
        self._running: set[subprocess.Popen[bytes]] = set()
        self._closed = False

    def start(self, command: list[str], session: Session) -> subprocess.Popen[bytes]:
        """Start COMMAND for SESSION in a new process group, logging its output."""
        with self._lock:
            if self._closed:
                raise InterruptedRunError
            with session.log.open("ab") as log:
                process = subprocess.Popen(
                    command, env=session.environment, stdin=subprocess.DEVNULL,
                    stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
            self._running.add(process)
        return process

    def stop(self, process: subprocess.Popen[bytes]) -> None:
        """Stop PROCESS, then kill every process it started that still runs.

        Emacs starts each subprocess (a language server, git, a native
        compiler) in a session of its own, outside the process group of
        Emacs, so they are found by their parent links before Emacs
        stops.  Emacs stops them itself on SIGTERM; one that survives,
        or one of an Emacs that had to be killed, is killed here.  Once
        PROCESS is reaped, its PID may name another process, so nothing
        is signalled through it any more.
        """
        family = {}
        if process.poll() is None:
            family = descendants(process.pid, process_rows())
            with contextlib.suppress(ProcessLookupError):
                os.killpg(process.pid, signal.SIGTERM)
            with contextlib.suppress(subprocess.TimeoutExpired):
                process.wait(timeout=STOP_SECONDS)
        if process.poll() is None:
            with contextlib.suppress(ProcessLookupError):
                os.killpg(process.pid, signal.SIGKILL)
        process.wait()
        for pid, _, start in process_rows():
            if family.get(pid) == start:
                with contextlib.suppress(ProcessLookupError):
                    os.kill(pid, signal.SIGKILL)
        with self._lock:
            self._running.discard(process)

    def stop_all(self) -> None:
        """Refuse new processes and stop every running one."""
        with self._lock:
            self._closed = True
            running = list(self._running)
        for process in running:
            self.stop(process)


def read_lines(path: Path) -> list[str]:
    """Return the complete lines of the result file PATH, if it exists."""
    if not path.exists():
        return []
    text = path.read_text(encoding="utf-8", errors="replace")
    # Not splitlines(): json-serialize writes U+2028 and the like raw.
    return text.split("\n")[:-1]


def client(session: Session, arguments: list[str], deadline: float) -> str:
    """Run emacsclient with ARGUMENTS against SESSION; return its output."""
    command = [EMACSCLIENT, f"--socket-name={session.socket}", *arguments]
    try:
        result = subprocess.run(
            command, env=session.environment, capture_output=True, text=True,
            timeout=max(1.0, deadline - time.monotonic()), check=False)
    except subprocess.TimeoutExpired as error:
        message = f"emacsclient {arguments[0]} timed out"
        raise SessionError(message) from error
    if result.returncode:
        message = f"emacsclient {' '.join(arguments)}: {result.stderr.strip()}"
        raise SessionError(message)
    return result.stdout.strip()


def wait_for(condition: Callable[[], bool], what: str, deadline: float,
             process: subprocess.Popen[bytes]) -> None:
    """Poll CONDITION until it holds; fail at DEADLINE or when PROCESS ends."""
    while not condition():
        if process.poll() is not None:
            message = f"Emacs exited with status {process.returncode} before {what}"
            raise SessionError(message)
        if time.monotonic() > deadline:
            message = f"deadline exceeded while waiting for {what}"
            raise SessionError(message)
        time.sleep(POLL_SECONDS)


def run_scenario(scenario: Scenario, directory: Path, reaper: Reaper, *,
                 graphic: bool) -> Outcome:
    """Run SCENARIO in a fresh daemon in DIRECTORY.

    The daemon starts as `emacs --fg-daemon'.  Once startup has finished,
    the runner creates a client frame on the session's own private
    display when GRAPHIC is true, and opens the first-file fixture if
    the file declares one.
    The specs start once the one-shot idle timers pending at that point
    have run, so they see what a user sees when Emacs has gone idle.
    """
    path, headers = scenario.path, scenario.headers
    started = time.monotonic()
    deadline = started + (L3_SECONDS if headers.l3 else SCENARIO_SECONDS)
    with private_display() if graphic else contextlib.nullcontext() as display:
        session = prepare(directory, display)
        process = reaper.start([EMACS, f"--fg-daemon={SERVER_NAME}",
                                f"--init-directory={session.init}"], session)

        def evaluates_to_t(form: str) -> Callable[[], bool]:
            return lambda: client(session, ["--eval", form], deadline) == "t"

        error = None
        try:
            wait_for(session.socket.exists, "the server socket", deadline, process)
            wait_for(evaluates_to_t("my/test-session-started"), "the end of startup",
                     deadline, process)
            opened = []
            if headers.first_file:
                fixture = Path(headers.first_file)
                source = TESTS / "fixtures" / fixture.parts[0]
                copy = directory / "sandbox" / fixture.parts[0]
                if source.is_dir():
                    shutil.copytree(source, copy, symlinks=True)
                else:
                    copy.parent.mkdir()
                    shutil.copy2(source, copy)
                repository = copy if source.is_dir() else copy.parent
                subprocess.run([GIT, "init", "-q", str(repository)],
                               env=session.environment, check=True, capture_output=True)
                opened = [str(directory / "sandbox" / fixture)]
            arguments = ["--no-wait", *opened]
            shown = [f"(get-file-buffer {elisp_string(file)})" for file in opened]
            if display:
                arguments += ["--create-frame"]
                shown += ["(frame-live-p my/test-session-frame)"]
            if shown:
                client(session, arguments, deadline)
                wait_for(evaluates_to_t(f"(and {' '.join(shown)} t)"),
                         "the client frame and first file", deadline, process)
            client(session, ["--eval", "(my/test-session-note-idle-timers)"], deadline)
            wait_for(evaluates_to_t("(my/test-session-settled-p)"),
                     "the pending idle timers", deadline, process)
            only = elisp_string(scenario.only) if scenario.only else "nil"
            run = (f"(run-at-time 0 nil #'my/test-session-run-files"
                   f" '({elisp_string(path)}) {elisp_string(session.results)} {only})")
            client(session, ["--eval", run], deadline)
            wait_for(lambda: bool(accounting.parse_report(
                         read_lines(session.results))[0]["done"]),
                     "the end of the specs", deadline, process)
        except (SessionError, OSError, subprocess.CalledProcessError) as caught:
            error = str(caught)
        finally:
            save = SAVE_MESSAGES.format(path=elisp_string(directory / "messages.txt"))
            with contextlib.suppress(SessionError):
                client(session, ["--eval", save], time.monotonic() + 5)
            reaper.stop(process)
    return Outcome([str(path)], read_lines(session.results), error,
                   str(session.socket), display, directory, time.monotonic() - started)


def run_logic(paths: list[Path], directory: Path, reaper: Reaper, *,
              seconds: float) -> Outcome:
    """Run the logic-test files PATHS in one batch Emacs in DIRECTORY.

    Startup follows the real order: early-init, init (with
    `after-init-time' unset, as during a real init), after-init-hook,
    emacs-startup-hook, window-setup-hook.  A batch Emacs draws no
    frame, so the session has no display.
    """
    started = time.monotonic()
    session = prepare(directory, None)
    files = " ".join(elisp_string(p) for p in paths)
    process = reaper.start([
        EMACS, "--batch", f"--init-directory={session.init}",
        "--eval", "(setq after-init-time nil)",
        "--eval", f"(load {elisp_string(session.init / 'early-init.el')} nil t t)",
        "--eval", f"(load {elisp_string(session.init / 'init.el')} nil t t)",
        "--eval", "(setq after-init-time (current-time))",
        "--eval", "(run-hooks 'after-init-hook 'emacs-startup-hook 'window-setup-hook)",
        "--eval", (f"(my/test-session-run-files '({files})"
                   f" {elisp_string(session.results)})"),
        "--eval", "(kill-emacs 0)",
    ], session)
    error = None
    try:
        process.wait(timeout=seconds)
    except subprocess.TimeoutExpired:
        error = f"deadline of {seconds:.0f} s exceeded; see {session.log}"
    finally:
        reaper.stop(process)
    if error is None and process.returncode:
        error = f"Emacs exited with status {process.returncode}; see {session.log}"
    return Outcome([str(p) for p in paths], read_lines(session.results), error,
                   str(session.socket), None, directory, time.monotonic() - started)
