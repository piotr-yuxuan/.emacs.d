;;; cheatsheet.el --- Bindings for the cheatsheet -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

(keymap-set t1fx-dangling-map "C-c l" (lambda () nil))
(keymap-set t1fx-clean-map "C-c k x" #'t1fx-command)
(keymap-set t1fx-dangling-map "C-c b" 't1fx-missing)
(keymap-set t1fx-clean-map "C-c a" #'t1fx-missing)
(keymap-set t1fx-clean-map "C-c a" #'t1fx-command)
(keymap-set t1fx-dangling-map "C-c d" 't1fx-ghost)
(use-package t1fx-library
  :bind-keymap ("C-c K" . t1fx-clean-map))
(keymap-set t1fx-clean-map "C-c s" #'t1fx-keyed)
(keymap-set t1fx-not-loaded-map "C-c n" #'t1fx-command)

;;; cheatsheet.el ends here
