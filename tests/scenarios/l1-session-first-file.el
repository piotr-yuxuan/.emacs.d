;;; l1-session-first-file.el --- Self-test of the first-file startup -*- lexical-binding: t -*-
;; scenario-startup: first-file startup-file/notes.el

;;; Commentary:
;; A file declaring `scenario-startup: first-file FIXTURE' gets the
;; buffer the runner opens right after startup, as `emacsclient FILE'
;; does for a user, before Emacs has gone idle.  The runner copies the
;; fixture's first directory into the session's sandbox, makes the copy
;; a git repository, and opens the copy, never the fixture itself.
;;
;; This file keeps that startup verifiable, with a graphical frame and
;; under EMACS_TEST_GUI=0, with a file that starts no language server.

;;; Code:

(require 'scenario)

(scenario
 :describe "SL-FIRST-FILE: the window shows the sandbox copy opened at startup"
 :given ((:name "opened at startup" :existing-buffer "\\`notes\\.el\\'"))
 :when ((:open))
 :then ((:mode emacs-lisp-mode)
        (:file-suffix "/sandbox/startup-file/notes.el")
        (:buffer-matches "^(defun startup-file-greeting ()$")))

(provide 'l1-session-first-file)

;;; l1-session-first-file.el ends here
