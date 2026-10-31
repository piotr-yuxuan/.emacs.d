"""Probe the graphical frames of two Emacs daemons, for plan 104, step 1.

The daemons start as tests/runner starts sessions: each in a session of
its own, with stdin from /dev/null.  On macOS they get no DISPLAY, so
their client frames are Cocoa frames on the runner's screen.  Each
opens one client frame at the same place, the second without taking
the focus.  The probe prints one JSON object per line: the window
server, the frames, the clipboard, timers and redisplay.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import time

EMACS = shutil.which("emacs") or "emacs"
EMACSCLIENT = shutil.which("emacsclient") or "emacsclient"
LAUNCHCTL = shutil.which("launchctl")
START_SECONDS = 30
SETTLE_SECONDS = 2
PLACE = "(left . 100) (top . 100) (width . 95) (height . 45)"
FRAMES = {
    "a": f"({PLACE})",
    "b": f"({PLACE} (no-focus-on-map . t) (no-accept-focus . t))",
}
# Every form returns a JSON text, which emacsclient prints as a string.
FRAME = "(seq-find #'display-graphic-p (frame-list))"
FACTS = f"""(let ((f {FRAME}))
  (json-encode
   (if (not f) '(:frame nil)
     (list :window-system (framep f)
           :display (frame-parameter f 'display)
           :env-display (getenv "DISPLAY")
           :visible (frame-visible-p f)
           :focus (frame-focus-state f)
           :chars (vector (frame-width f) (frame-height f))
           :pixels (vector (frame-pixel-width f) (frame-pixel-height f))
           :char-size (vector (frame-char-width f) (frame-char-height f))
           :scale (frame-scale-factor f)
           :screen (vector (display-pixel-width f) (display-pixel-height f))
           :font (face-attribute 'default :family f)))))"""
SHOW_TEXT = f"""(with-selected-frame {FRAME}
  (switch-to-buffer (get-buffer-create "probe"))
  (emacs-lisp-mode)
  (erase-buffer)
  (insert "(defun probe () \\"a string\\")")
  (goto-char (point-min))
  (json-encode t))"""
FONTIFIED = """(progn
  (redisplay t)
  (json-encode
   (list :fontified (get-text-property 1 'fontified (get-buffer "probe"))
         :face (get-text-property 2 'face (get-buffer "probe")))))"""
START_TIMER = """(progn
  (defvar probe-late nil)
  (let ((start (float-time)))
    (run-with-timer 0.1 nil (lambda () (setq probe-late (- (float-time) start 0.1)))))
  (json-encode t))"""
TIMER_LATE = "(json-encode (list :late probe-late))"
SET_CLIPBOARD = "(progn (gui-set-selection 'CLIPBOARD \"probe-a\") (json-encode t))"
GET_CLIPBOARD = "(json-encode (list :clipboard (gui-get-selection 'CLIPBOARD)))"


def report(fact: str, value: object) -> None:
    """Print FACT and its VALUE as one line of JSON."""
    sys.stdout.write(json.dumps({"fact": fact, "value": value}) + "\n")
    sys.stdout.flush()


def client(name: str, *arguments: str) -> subprocess.CompletedProcess[str]:
    """Run emacsclient against the daemon NAME with ARGUMENTS."""
    return subprocess.run(
        [EMACSCLIENT, f"--socket-name={name}", *arguments],
        capture_output=True, text=True, timeout=START_SECONDS, check=False)


def evaluate(name: str, form: str) -> object:
    """Evaluate FORM in the daemon NAME; return its JSON, or the error."""
    result = client(name, "--eval", form)
    if result.returncode:
        return {"error": result.stderr.strip()}
    return json.loads(json.loads(result.stdout, strict=False))


def start(name: str, environment: dict[str, str]) -> subprocess.Popen[bytes]:
    """Start the daemon NAME, wait until it answers, and load json.el."""
    daemon = subprocess.Popen(
        [EMACS, "-Q", f"--fg-daemon={name}"], stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        env=environment, start_new_session=True)
    deadline = time.monotonic() + START_SECONDS
    while client(name, "--eval", "t").returncode and time.monotonic() < deadline:
        time.sleep(0.5)
    client(name, "--eval", "(require 'json)")
    return daemon


def main() -> None:
    """Start two daemons, open their frames, and report what they show."""
    if LAUNCHCTL:
        report("launchctl managername", subprocess.run(
            [LAUNCHCTL, "managername"], capture_output=True, text=True,
            check=False).stdout.strip())
    report("emacs", EMACS)
    report("emacsclient", EMACSCLIENT)
    environment = dict(os.environ)
    if sys.platform == "darwin":
        environment.pop("DISPLAY", None)
    daemons = {name: start(name, environment) for name in FRAMES}
    try:
        for name, parameters in FRAMES.items():
            created = client(name, "-c", "-n", "-F", parameters)
            report(f"{name}: frame", created.stderr.strip() or "made")
            evaluate(name, SHOW_TEXT)
            evaluate(name, START_TIMER)
        evaluate("a", SET_CLIPBOARD)
        time.sleep(SETTLE_SECONDS)
        for name in FRAMES:
            report(f"{name}: facts", evaluate(name, FACTS))
            report(f"{name}: redisplay", evaluate(name, FONTIFIED))
            report(f"{name}: timer", evaluate(name, TIMER_LATE))
        report("b: clipboard set by a", evaluate("b", GET_CLIPBOARD))
    finally:
        for name, daemon in daemons.items():
            client(name, "--eval", "(kill-emacs)")
            daemon.wait(timeout=START_SECONDS)


if __name__ == "__main__":
    main()
