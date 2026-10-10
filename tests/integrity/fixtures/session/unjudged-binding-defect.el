;;; unjudged-binding-defect.el --- Bindings no check can judge, unexplained -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; No loaded code defines t1fx-unloaded-map, and t1fx-absent-command
;; autoloads from a library that does not exist (live.el); the self-test
;; skips no package.
(keymap-set t1fx-unloaded-map "C-c a" #'t1fx-command)
(keymap-global-set "C-c n" #'t1fx-absent-command)

;;; unjudged-binding-defect.el ends here
