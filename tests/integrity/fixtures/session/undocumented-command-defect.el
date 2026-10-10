;;; undocumented-command-defect.el --- A key bound to a command without a docstring -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

(keymap-set t1fx-clean-map "C-c a" #'t1fx-command)
(keymap-set t1fx-clean-map "C-c u" #'t1fx-undocumented)

;;; undocumented-command-defect.el ends here
