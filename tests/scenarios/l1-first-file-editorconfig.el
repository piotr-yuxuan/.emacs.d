;;; l1-first-file-editorconfig.el --- The first file gets its editorconfig settings -*- lexical-binding: t -*-
;; scenario-startup: first-file editorconfig-first-file/notes.el

;;; Commentary:
;; 084 DD-04: editorconfig started from an idle timer, so a file opened
;; by emacsclient right after startup, before Emacs had been idle,
;; missed its .editorconfig settings.  The runner opens this fixture as
;; soon as startup ends: its .editorconfig sets a 77-column fill and
;; tabs, which only an editorconfig active at that moment applies.
;;
;; The PATH half of DD-04 is not checked here: the session inherits the
;; runner's PATH, so the first file's hooks would see the login PATH
;; whether exec-path-from-shell ran during init or not.

;;; Code:

(require 'scenario)

(scenario
 :describe "DD-04: the first file of a session gets its editorconfig settings"
 :given ((:name "first file" :existing-buffer "\\`notes\\.el\\'"))
 :when ((:open))
 :then ((:variable-equals fill-column 77)
        (:variable-equals indent-tabs-mode t)))

(provide 'l1-first-file-editorconfig)

;;; l1-first-file-editorconfig.el ends here
