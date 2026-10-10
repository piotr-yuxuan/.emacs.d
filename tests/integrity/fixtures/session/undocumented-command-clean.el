;;; undocumented-command-clean.el --- Keys bound to documented commands -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; An undefined command is the dangling-binding check's to report.
(keymap-set t1fx-clean-map "C-c a" #'t1fx-command)
(keymap-set t1fx-clean-map "C-c m" #'t1fx-missing)

;;; undocumented-command-clean.el ends here
