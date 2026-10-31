;;; hang-in-prompt.el --- Self-test fixture: a spec that waits for an answer -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 106.  Not a scenario file: tests/runner/test_sessions.py runs
;; it with a short deadline.  Its one spec asks a question that no key
;; answers, as lsp-mode does when a language server exits, so the
;; session waits until the runner gives up, and must then say what it
;; waited for.

;;; Code:

(describe "HANG"
  (it "waits for an answer nobody gives"
    (y-or-n-p "Restart the probe server? ")))

;;; hang-in-prompt.el ends here
