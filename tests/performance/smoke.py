#!/usr/bin/env python3
"""Check dashboard, file arguments, commands and refresh in private GUI frames."""
import argparse
import json
from pathlib import Path
import subprocess
import time

from run import LIVE, ROOT, client_series, elisp_string, launch, measurement_display, prepare
from runtime import evaluate


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--user-display", action="store_true",
                        help="draw frames on the inherited display, not on Xvfb")
    args = parser.parse_args()
    folder = args.output.resolve()
    folder.mkdir(parents=True, exist_ok=False)
    sample = folder / "sample.el"
    sample.write_text("(message \"disposable startup fixture\")\n")
    with measurement_display(user_display=args.user_display):
        checks = []
        for with_file in (False, True):
            part = folder / ("file" if with_file else "dashboard")
            part.mkdir()
            session = prepare(args.source.resolve(), part / "session", LIVE)
            report, process = launch(session, part, 0, keep=True,
                                     arguments=[sample] if with_file else [])
            socket = Path((session / "socket-directory").read_text()) / "perf-session"
            try:
                deadline = time.monotonic() + 15
                while not socket.exists():
                    if time.monotonic() > deadline:
                        raise TimeoutError("Private server unavailable")
                    time.sleep(.05)
                # Emacs 31 shows initial-buffer-choice beside a file argument
                # and focuses it. Verify the file is visible, not discarded.
                assert report["mode"] == "dashboard-mode", report
                if with_file:
                    value = evaluate(socket, f'''(let ((buffer (get-file-buffer {elisp_string(sample)})))
                      (and buffer (get-buffer-window buffer t)
                           (with-current-buffer buffer (derived-mode-p 'emacs-lisp-mode)) t))''')
                    assert value == "t", value
                assert report["frame_visible"], report
                checks.append({"name": "file argument" if with_file else "dashboard startup", "passed": True,
                               "render_count": report["dashboard_renders"]})
                value = evaluate(socket, '''(progn
                  (delete-other-windows) (execute-kbd-macro (kbd "C-x 2"))
                  (unless (= (length (window-list)) 2) (error "Split command failed"))
                  (execute-kbd-macro (kbd "C-x 1"))
                  (dashboard-open) (execute-kbd-macro (kbd "g")) (redisplay t)
                  (unless (and (derived-mode-p 'dashboard-mode) (null dashboard-items)
                               (string-match-p "Welcome to Emacs" (buffer-string)))
                    (error "Empty dashboard refresh failed"))
                  (set-frame-width (selected-frame) (1+ (frame-width)))
                  (redisplay t) t)''')
                assert value == "t", value
                checks.append({"name": "commands, refresh and resize", "passed": True})
            finally:
                try:
                    evaluate(socket, "(kill-emacs 0)", timeout=5)
                except (RuntimeError, subprocess.TimeoutExpired):
                    process.terminate()
                process.wait(timeout=10)
        part = folder / "clients"
        part.mkdir()
        session = prepare(args.source.resolve(), part / "session", LIVE)
        reports = client_series(session, part, 2, 0)
        for index, report in enumerate(reports):
            assert report["mode"] == "dashboard-mode" and report["frame_visible"], report
            checks.append({"name": f"client frame {index + 1}", "passed": True,
                           "render_count": report["dashboard_renders"]})
        part = folder / "client-file"
        part.mkdir()
        session = prepare(args.source.resolve(), part / "session", LIVE)
        report = client_series(session, part, 1, 0, [sample])[0]
        assert report["mode"] == "emacs-lisp-mode" and report["frame_visible"], report
        checks.append({"name": "client file argument", "passed": True})
        (folder / "checks.json").write_text(json.dumps(checks, indent=2) + "\n")
        print(json.dumps(checks, indent=2))


if __name__ == "__main__":
    main()
