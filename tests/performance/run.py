#!/usr/bin/env python3
"""Isolated Emacs startup measurements; no changes to the live configuration."""

import argparse
from collections.abc import Iterator
import contextlib
import hashlib
import json
import math
import os
from pathlib import Path
import shutil
import statistics
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
LIVE = Path(os.environ.get("EMACS_LIVE_DIR", "~/.emacs.d")).expanduser()


def elisp_string(value):
    return json.dumps(str(value))


@contextlib.contextmanager
def measurement_display(*, user_display: bool) -> Iterator[None]:
    """Give every Emacs this process starts a private display, unless USER_DISPLAY.

    The private display is the test runner's Xvfb server, so no frame
    reaches the user's screen.  It is X11 under GTK, not the user's
    Wayland: compare its numbers with each other only.  USER_DISPLAY
    keeps the inherited display, for a deliberate measurement there.
    """
    if user_display:
        yield
        return
    sys.path.insert(0, str(ROOT / "tests"))
    from runner import sessions
    if sessions.XVFB is None or not sessions.emacs_features() & sessions.X_FEATURES:
        message = ("No private display: Xvfb is missing or this Emacs cannot draw"
                   " on X.  Pass --user-display to measure on your own screen.")
        raise SystemExit(message)
    with sessions.private_display() as display:
        os.environ.pop("WAYLAND_DISPLAY", None)
        os.environ.update(DISPLAY=display, GDK_BACKEND="x11")
        yield


def prepare(source, session, packages, minimal=False):
    """Create a private init directory that reads the packages under PACKAGES."""
    session.mkdir(parents=True, exist_ok=True)
    for name in ("var", "etc"):
        if not (session / name).exists():
            shutil.copytree(LIVE / name, session / name, symlinks=True,
                            ignore=lambda folder, names: [n for n in names if (Path(folder) / n).is_socket()])
    # No running server's socket is copied or reused.
    for name in ("tree-sitter",):
        if not (session / name).exists() and (LIVE / name).exists():
            (session / name).symlink_to(LIVE / name, target_is_directory=True)
    if (LIVE / "local.el").exists():
        shutil.copy2(LIVE / "local.el", session / "local.el")
    # Unix sockets have a short path limit; a nested worktree exceeds it.
    socket_dir = Path(tempfile.mkdtemp(prefix="ep036-"))
    (session / "socket-directory").write_text(str(socket_dir))
    # As in the test runner: loading a package such as magit runs git in
    # its straight.el repository (`git describe --dirty' takes the index
    # lock).  Git climbs from no subdirectory of a live repository, where
    # the builds' files link to; it still finds one from its root.
    repositories = sorted((LIVE / "straight/repos").glob("*"))
    ceilings = os.pathsep.join(str(p) for p in repositories)
    setup = f''';;; -*- lexical-binding: t; -*-
(setq straight-base-dir {elisp_string(str(packages) + '/')}
      straight-build-dir {elisp_string(LIVE / 'straight/build')}
      straight-build-cache-fixed-name "build-cache.el"
      straight-safe-mode t
      server-name {elisp_string('perf-' + session.name)}
      server-socket-dir {elisp_string(socket_dir)})
(setenv "GIT_CEILING_DIRECTORIES" {elisp_string(ceilings)})
(with-eval-after-load 'mcp-server
  (setq mcp-server-socket-directory {elisp_string(socket_dir / 'mcp')}))
(setq native-comp-eln-load-path
      (append (list {elisp_string(str(session / 'eln-cache'))}
                    {elisp_string(str(LIVE / 'eln-cache'))}) native-comp-eln-load-path))
(load {elisp_string(ROOT / 'tests/performance/startup.el')} nil t)
'''
    early = "(setq package-enable-at-startup nil default-frame-alist '((width . 95) (height . 45)))\n" if minimal else f"(load {elisp_string(source / 'early-init.el')} nil t)\n"
    (session / "early-init.el").write_text(setup + early)
    # The injected --init-directory is harness plumbing. Dashboard must see
    # the logical launch arguments, otherwise its no-argument startup hook
    # silently disappears from the measurement.
    init = "" if minimal else f'''(let ((command-line-args
       (cons (car command-line-args) command-line-args-left)))
  (load {elisp_string(source / 'init')} nil t))
;; Keep language servers out of projects remembered by the live session.
(setq lsp-session-file {elisp_string(session / 'isolated-lsp-session')}
      lsp--session nil
      lsp-auto-guess-root t
      lsp-guess-root-without-session t)
(with-eval-after-load 'lsp-mode
  (setq lsp-session-file {elisp_string(session / 'isolated-lsp-session')}
        lsp--session nil
        lsp-auto-guess-root t
        lsp-guess-root-without-session t))
'''
    (session / "init.el").write_text(";;; -*- lexical-binding: t; -*-\n" + init)
    return session


def manifest(source, minimal=False):
    "Describe the exact source and installed cache used by an experiment."
    def command(*args):
        result = subprocess.run(args, text=True, capture_output=True)
        return result.stdout.strip() if result.returncode == 0 else None
    return {
        "recorded_epoch": time.time(),
        "configuration_commit": (source / "BASELINE_COMMIT").read_text().strip()
            if (source / "BASELINE_COMMIT").exists()
            else command("git", "-C", str(source), "rev-parse", "HEAD"),
        "source_sha256": {name: hashlib.sha256((source / name).read_bytes()).hexdigest()
                          for name in ("README.org", "early-init.el", "init.el") if (source / name).exists()},
        "minimal": minimal,
        "emacs_binary": str(Path(shutil.which("emacs")).resolve()),
        "emacs_version": command("emacs", "--version"),
        "platform": sys.platform,
        "requested_backend": os.environ.get("EMACS_PERF_BACKEND", "wayland")
            if sys.platform.startswith("linux") and os.environ.get("WAYLAND_DISPLAY")
            else os.environ.get("GDK_BACKEND"),
        "display": os.environ.get("WAYLAND_DISPLAY") or os.environ.get("DISPLAY"),
        "native_cache": {"read_only": str(LIVE / "eln-cache"),
                         "eln_files": len(list((LIVE / "eln-cache").rglob("*.eln")))},
        "compiled_init": (source / "init.elc").exists(),
        "package_revisions": {p.name: command("git", "-C", str(p), "rev-parse", "HEAD")
                              for p in sorted((LIVE / "straight/repos").iterdir()) if (p / ".git").exists()},
        "instrumentation": "ready command after redisplay; no sampling unless --profile",
        "client_boundary": "includes a second emacsclient evaluation and redisplay",
    }


def quantile(values, fraction):
    return sorted(values)[max(0, math.ceil(len(values) * fraction) - 1)]


def client_series(session, folder, samples, warmups, arguments=()):
    """Measure client frames against a private daemon, including first frame."""
    from runtime import evaluate
    daemon_folder = folder / "daemon"
    daemon_folder.mkdir(exist_ok=True)
    _, process = launch(session, daemon_folder, 0, keep=True, daemon=True)
    socket = Path((session / "socket-directory").read_text()) / f"perf-{session.name}"
    results = []
    try:
        deadline = time.monotonic() + 15
        while not socket.exists():
            if time.monotonic() > deadline:
                raise TimeoutError("Private daemon socket not available")
            time.sleep(.05)
        evaluate(socket, "(add-hook 'after-make-frame-functions (lambda (frame) (setq perf-client-frame frame)))")
        for index in range(samples + warmups):
            ready = folder / f"client-{index:03d}.json"
            evaluate(socket, f"(progn (setq perf-startup-stages nil perf-dashboard-renders 0) (setenv \"EMACS_PERF_READY\" {elisp_string(ready)}))")
            start = time.time()
            subprocess.run(["emacsclient", "--socket-name", str(socket), "--create-frame", "--no-wait", *map(str, arguments)], check=True, capture_output=True, timeout=20)
            evaluate(socket, "(with-selected-frame perf-client-frame (with-current-buffer (window-buffer (frame-selected-window perf-client-frame)) (perf-ready-command)))")
            report = json.loads(ready.read_text())
            report.update(ready_seconds=report["stages"]["ready"] - start, warmup=index < warmups)
            ready.write_text(json.dumps(report, indent=2) + "\n")
            results.append(report)
            print(f"client {index}: ready={report['ready_seconds']:.4f}s", flush=True)
            evaluate(socket, "(delete-frame perf-client-frame t)")
    finally:
        try:
            evaluate(socket, "(kill-emacs 0)", timeout=5)
        except (RuntimeError, subprocess.TimeoutExpired):
            process.terminate()
        process.wait(timeout=10)
    return results


def launch(session, folder, index, profile=False, keep=False, daemon=False, arguments=()):
    ready = folder / f"{index:03d}.json"
    if ready.exists():
        raise FileExistsError(f"Refusing stale readiness marker: {ready}")
    log = folder / f"{index:03d}.log"
    env = os.environ.copy()
    if sys.platform.startswith("linux") and env.get("WAYLAND_DISPLAY"):
        env["GDK_BACKEND"] = os.environ.get("EMACS_PERF_BACKEND", "wayland")
    env["EMACS_PERF_READY"] = str(ready)
    env.pop("EMACS_PERF_EXIT", None)
    env.pop("EMACS_PERF_PROFILE", None)
    if not keep:
        env["EMACS_PERF_EXIT"] = "1"
    if profile:
        env["EMACS_PERF_PROFILE"] = "1"
    command = [shutil.which("emacs"), "--init-directory", str(session)]
    if daemon:
        command.append(f"--fg-daemon=perf-{session.name}")
    command.extend(map(str, arguments))
    start = time.time()
    with log.open("w") as output:
        process = subprocess.Popen(command, env=env, stdout=output, stderr=output,
                                   start_new_session=True)
    try:
        deadline = time.monotonic() + 25
        while not ready.exists():
            if process.poll() is not None:
                raise RuntimeError(f"Emacs exited {process.returncode}; see {log}")
            if time.monotonic() > deadline:
                raise TimeoutError(f"No readiness event; see {log}")
            time.sleep(0.01)
        report = json.loads(ready.read_text())
        report.update(launch_epoch=start, ready_seconds=report["stages"]["ready"] - start)
        ready.write_text(json.dumps(report, indent=2) + "\n")
        if not keep:
            process.wait(timeout=10)
        return report, process
    except BaseException:  # an interrupted run leaves no daemon behind
        process.terminate()
        process.wait(timeout=10)
        raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--packages", type=Path, default=LIVE,
                        help="the directory holding straight/, read in safe mode")
    parser.add_argument("--samples", type=int, default=30)
    parser.add_argument("--warmups", type=int, default=2)
    parser.add_argument("--minimal", action="store_true")
    parser.add_argument("--profile", action="store_true")
    parser.add_argument("--client", action="store_true")
    parser.add_argument("--user-display", action="store_true",
                        help="draw frames on the inherited display, not on Xvfb")
    args = parser.parse_args()
    if args.samples < 1 or args.warmups < 0:
        parser.error("samples must be positive and warmups nonnegative")
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=False)
    with measurement_display(user_display=args.user_display):
        (args.output / "manifest.json").write_text(json.dumps(manifest(args.source.resolve(), args.minimal), indent=2) + "\n")
        session = prepare(args.source.resolve(), args.output / "session", args.packages.resolve(), args.minimal)
        reports, failures = [], []
        if args.client:
            reports = client_series(session, args.output, args.samples, args.warmups)
        for i in range(0 if args.client else args.samples + args.warmups):
            try:
                report, _ = launch(session, args.output, i, args.profile)
                report["warmup"] = i < args.warmups
                (args.output / f"{i:03d}.json").write_text(json.dumps(report, indent=2) + "\n")
                reports.append(report)
                print(f"{i}: ready={report['ready_seconds']:.4f}s init={report['init_seconds']:.4f}s", flush=True)
            except Exception as error:
                failures.append({"index": i, "error": str(error)})
                print(str(error), flush=True)
                if len(failures) >= 3:
                    break
        measured = [r["ready_seconds"] for r in reports if not r["warmup"]]
        summary = {"samples": len(measured), "failures": failures, "profiled": args.profile}
        if measured:
            summary.update(median=statistics.median(measured), p95=quantile(measured, .95), maximum=max(measured))
        (args.output / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
        print(json.dumps(summary, indent=2))
    return bool(failures)


if __name__ == "__main__":
    raise SystemExit(main())
