;;; unjudged-binding-clean.el --- Bindings judged, or of a skipped package -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; The self-test skips t1fx-skipped, which would define
;; t1fx-skipped-map, and t1fx-absent-library, which t1fx-absent-command
;; autoloads from (live.el).
(use-package t1fx-skipped
  :config (keymap-set t1fx-skipped-map "C-c a" #'t1fx-command))
(keymap-global-set "C-c n" #'t1fx-absent-command)
(keymap-set t1fx-clean-map "C-c a" #'t1fx-command)

;;; unjudged-binding-clean.el ends here
