#!/usr/bin/env python3
"""Time the first C-x g, C-x b, C-s and M-. in a client frame of a fresh private daemon."""

import argparse
import json
import os
import statistics
import subprocess
import time
from pathlib import Path

from run import LIVE, ROOT, elisp_string, launch, manifest, measurement_display, prepare
from runtime import evaluate

# Each key, the keys that close what it opens, and what Emacs shows once
# the key is done.  README.org binds them; D11 keeps C-s on consult-line.
COMMANDS = {
    "C-x g": ("", "(eq major-mode 'magit-status-mode)"),
    "C-x b": ("C-g", '(equal (minibuffer-prompt) "Switch to: ")'),
    "C-s": ("C-g", '(equal (minibuffer-prompt) "Go to line: ")'),
    "M-.": ("", '(looking-at-p "(defun perf-first-use-target")'),
}
FIXTURE = """;;; probe.el --- First-use fixture -*- lexical-binding: t; -*-
(defun perf-first-use-target (x)
  "Return X plus one."
  (1+ x))

(perf-first-use-target 1)
"""
# The probe event is read where Emacs waits for the next key, inside a
# prompt too, and translates to no key, as in tests/helpers/keystrokes.el.
# The fixture is loaded: the elisp xref backend reads `load-history'.
SETUP = """(progn
  (require 'cl-lib)
  (defvar perf-first-use nil)
  (defvar perf-first-use-check nil)
  (keymap-set key-translation-map "<perf-first-use-probe>"
    (lambda (_prompt)
      ;; While a keyboard macro runs, `redisplay' does nothing.
      (let ((executing-kbd-macro nil)) (redisplay t))
      (setq perf-first-use (list :stop (float-time) :features features :gc gc-elapsed
                                 :passed (funcall perf-first-use-check)))
      []))
  (load {fixture} nil t)
  (with-selected-frame perf-client-frame
    (with-current-buffer (window-buffer)
      (goto-char (point-max))
      (search-backward "perf-first-use-target")
      (set-window-point nil (point))
      (redisplay t)
      major-mode)))"""
# A pressed key ends the idle period, so no idle timer runs during the
# command (`my/scenario-as-user').  The time runs from the key to the
# probe; C-g then closes a prompt.
MEASURE = """(with-selected-frame perf-client-frame
  (when (current-idle-time)
    (let ((special-event-map
           (define-keymap "<sigusr1>" (lambda () (interactive) (throw 'perf-acting nil)))))
      (catch 'perf-acting (signal-process (emacs-pid) 'sigusr1) (read-event))))
  (setq perf-first-use nil perf-first-use-check (lambda () {check}))
  (let ((keys (vconcat (key-parse {key}) [perf-first-use-probe] (key-parse {close})))
        (before features) (gc gc-elapsed) (failure nil) (start (float-time)))
    (condition-case err (execute-kbd-macro keys)
      ((error quit) (setq failure (error-message-string err))))
    (let ((stop (plist-get perf-first-use :stop))
          (after (or (plist-get perf-first-use :features) features)))
      (with-temp-file {output}
        (insert (json-serialize
                 (list :key {key} :seconds (if stop (- stop start) :null)
                       :gc_seconds (if stop (- (plist-get perf-first-use :gc) gc) :null)
                       :passed (if (and (plist-get perf-first-use :passed) (not failure)) t :false)
                       :error (or failure :null)
                       :features_before (length before) :features_after (length after)
                       :loaded (vconcat (mapcar #'symbol-name (cl-ldiff after before))))))))))"""


def main() -> bool:
    """Measure each key on fresh daemons; return True when a sample failed."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--rounds", type=int, default=5)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--idle", type=float, default=2.0,
                        help="seconds the daemon idles, its frame shown, before the key")
    parser.add_argument("--user-display", action="store_true",
                        help="draw frames on the inherited display, not on Xvfb")
    args = parser.parse_args()
    if args.rounds < 1 or args.warmups < 0 or args.idle < 0:
        parser.error("rounds must be positive, warmups and idle nonnegative")
    folder = args.output.resolve()
    folder.mkdir(parents=True, exist_ok=False)
    repository = folder / "repository"
    repository.mkdir()
    fixture = repository / "probe.el"
    fixture.write_text(FIXTURE)
    # The test runner's git configuration: no signing, hooks or templates.
    git = dict(os.environ, GIT_CONFIG_GLOBAL=str(ROOT / "tests/runner/gitconfig"), GIT_CONFIG_NOSYSTEM="1")
    for command in (["init", "-q"], ["add", "probe.el"], ["commit", "-q", "-m", "Add the fixture"]):
        subprocess.run(["git", "-C", str(repository), *command], check=True, env=git)
    fixture.write_text(FIXTURE + ";; An unstaged change, for the status buffer.\n")
    daemons = folder / "daemons"
    daemons.mkdir()
    results = []
    with measurement_display(user_display=args.user_display):
        metadata = manifest(args.source.resolve())
        metadata.update(rounds=args.rounds, warmups=args.warmups, idle=args.idle)
        (folder / "manifest.json").write_text(json.dumps(metadata, indent=2) + "\n")
        session = prepare(args.source.resolve(), folder / "session", LIVE)
        socket = Path((session / "socket-directory").read_text()) / "perf-session"
        try:
            for round_index in range(args.warmups + args.rounds):
                # One fresh daemon per key: consult serves both C-x b and C-s.
                for key, (close, check) in COMMANDS.items():
                    index = len(results)
                    boot, process = launch(session, daemons, index, keep=True, daemon=True)
                    try:
                        evaluate(socket, "(add-hook 'after-make-frame-functions (lambda (frame) (setq perf-client-frame frame)))")
                        subprocess.run(["emacsclient", "--socket-name", str(socket), "--create-frame", "--no-wait", str(fixture)],
                                       check=True, capture_output=True, timeout=20)
                        mode = evaluate(socket, SETUP.format(fixture=elisp_string(fixture)))
                        if mode != "emacs-lisp-mode":
                            message = f"The client frame shows {mode}, not {fixture}"
                            raise RuntimeError(message)
                        time.sleep(args.idle)
                        output = daemons / f"{index:03d}-first-use.json"
                        evaluate(socket, MEASURE.format(key=elisp_string(key), close=elisp_string(close),
                                                        check=check, output=elisp_string(output)))
                        report = json.loads(output.read_text())
                    finally:
                        try:
                            evaluate(socket, "(kill-emacs 0)", timeout=5)
                        except (RuntimeError, subprocess.TimeoutExpired):
                            process.terminate()
                        process.wait(timeout=10)
                    report.update(round=round_index, warmup=round_index < args.warmups,
                                  boot_seconds=boot["ready_seconds"])
                    results.append(report)
                    print(json.dumps({name: report[name] for name in ("round", "key", "seconds", "passed", "error")}), flush=True)
        finally:
            summary = {}
            for key in COMMANDS:
                rows = [r for r in results if r["key"] == key and not r["warmup"]]
                times = [r["seconds"] for r in rows if r["passed"]]
                summary[key] = {"samples": len(times), "failures": len(rows) - len(times),
                                "median": statistics.median(times) if times else None,
                                "maximum": max(times, default=None),
                                "features_loaded": [len(r["loaded"]) for r in rows]}
            (folder / "first-use.json").write_text(json.dumps({"summary": summary, "results": results}, indent=2) + "\n")
            print(json.dumps(summary, indent=2))
    return any(not r["passed"] for r in results)


if __name__ == "__main__":
    raise SystemExit(main())

