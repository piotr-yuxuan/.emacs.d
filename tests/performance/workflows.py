#!/usr/bin/env python3
"""Opt-in real language-server and REPL checks in disposable GUI projects."""
import argparse
import json
from pathlib import Path
import subprocess
import time

from run import LIVE, ROOT, elisp_string, launch, manifest, measurement_display, prepare
from runtime import evaluate, fixtures


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--user-display", action="store_true",
                        help="draw frames on the inherited display, not on Xvfb")
    args = parser.parse_args()
    folder = args.output.resolve()
    folder.mkdir(parents=True, exist_ok=False)
    with measurement_display(user_display=args.user_display):
        (folder / "manifest.json").write_text(json.dumps(manifest(ROOT), indent=2))
        session = prepare(ROOT, folder / "session", LIVE)
        _, process = launch(session, folder, 0, keep=True)
        socket = Path((session / "socket-directory").read_text()) / "perf-session"
        results = []

        def record(name, expression, timeout=30):
            start = time.monotonic()
            try:
                value = evaluate(socket, expression, timeout)
                result = {"name": name, "seconds": time.monotonic() - start, "value": value}
            except (RuntimeError, subprocess.TimeoutExpired) as error:
                result = {"name": name, "seconds": time.monotonic() - start, "error": str(error)}
            results.append(result)
            (folder / "workflows.json").write_text(json.dumps(results, indent=2) + "\n")
            print(json.dumps(result), flush=True)
            return result

        try:
            deadline = time.monotonic() + 15
            while not socket.exists():
                if time.monotonic() > deadline:
                    raise TimeoutError("Private server unavailable")
                time.sleep(.1)
            paths = fixtures(folder / "fixtures")
            for path in paths:
                record(f"{path.name}:open", f"(progn (find-file {elisp_string(path)}) (redisplay t) major-mode)")
                if path.suffix == ".org":
                    record("Org:fold", "(progn (goto-char (point-min)) (outline-next-heading) (org-cycle) (org-cycle) (redisplay t) t)")
                    continue
                # Return to the actual command loop between readiness polls, so
                # deferred attach, process output and ordinary timers can run.
                start = time.monotonic()
                while time.monotonic() - start < 40:
                    ready = evaluate(socket, f'''(with-current-buffer (get-file-buffer {elisp_string(path)})
                      (and (bound-and-true-p lsp-mode) (lsp-workspaces)
                           (cl-every (lambda (w) (eq (lsp--workspace-status w) 'initialized)) (lsp-workspaces))))''')
                    if ready == "t":
                        break
                    time.sleep(.2)
                results.append({"name": f"{path.name}:server-ready", "seconds": time.monotonic() - start, "ready": ready == "t"})
                record(f"{path.name}:requests", f'''(with-current-buffer (get-file-buffer {elisp_string(path)})
                  (let ((lsp-response-timeout 5) (report nil))
                    (goto-char (point-min)) (forward-line 2) (end-of-line)
                    (dolist (spec '(("completion" "textDocument/completion")
                                    ("symbols" "textDocument/documentSymbol")
                                    ("formatting" "textDocument/formatting")))
                      (push (cons (car spec)
                        (condition-case err
                          (if (not (lsp-feature? (cadr spec))) 'unsupported
                            (let ((response (lsp-request (cadr spec)
                              (if (equal (car spec) "formatting")
                                (list :textDocument (lsp--text-document-identifier)
                                      :options (list :tabSize 2 :insertSpaces t))
                                (lsp--text-document-position-params)))))
                              (list :response (type-of response)
                                    :count (if (sequencep response) (length response) nil))))
                          (error (error-message-string err)))) report))
                    (list :workspaces (mapcar (lambda (w) (list (lsp--workspace-server-id w) (lsp--workspace-root w))) (lsp-workspaces))
                          :requests (nreverse report)
                          :diagnostics (length (bound-and-true-p flycheck-current-errors)))))''')
            py = next(p for p in paths if p.suffix == ".py")
            record("Python:start", f'''(progn (find-file {elisp_string(py)})
              (run-python (python-shell-calculate-command) nil nil)
              (accept-process-output (python-shell-get-process) 1) t)''')
            record("Python:REPL", "(python-shell-send-string-no-output \"print(6 * 7)\" (python-shell-get-process))")
            clj = next(p for p in paths if p.suffix == ".clj")
            record("CIDER:start", f'''(progn (find-file {elisp_string(clj)})
              (cider-jack-in-clj (list :project-dir default-directory :project-type 'clojure-cli)) t)''')
            start = time.monotonic()
            connected = False
            while time.monotonic() - start < 90:
                if evaluate(socket, f"(with-current-buffer (get-file-buffer {elisp_string(clj)}) (and (fboundp 'cider-connected-p) (cider-connected-p) t))") == "t":
                    connected = True
                    break
                time.sleep(.5)
            results.append({"name": "CIDER:ready", "seconds": time.monotonic() - start, "ready": connected})
            if connected:
                record("CIDER:REPL", f"(with-current-buffer (get-file-buffer {elisp_string(clj)}) (nrepl-dict-get (cider-nrepl-sync-request:eval \"(* 6 7)\") \"value\"))")
                record("CIDER:output", f"(with-current-buffer (get-file-buffer {elisp_string(clj)}) (nrepl-dict-get (cider-nrepl-sync-request:eval \"(dotimes [i 1000] (println i))\") \"status\"))")
            record("background", "(list :processes (mapcar (lambda (p) (list (process-name p) (process-status p))) (process-list)) :gc-time gc-elapsed :rss (cdr (assq 'rss (process-attributes (emacs-pid)))))")
        finally:
            (folder / "workflows.json").write_text(json.dumps(results, indent=2) + "\n")
            try:
                evaluate(socket, f'''(with-temp-file {elisp_string(folder / 'messages.txt')}
                  (insert (with-current-buffer "*Messages*" (buffer-string))))''', timeout=5)
                evaluate(socket, "(let ((kill-emacs-query-functions nil)) (kill-emacs 0))", timeout=5)
            except (RuntimeError, subprocess.TimeoutExpired):
                process.terminate()
            process.wait(timeout=10)


if __name__ == "__main__":
    main()
