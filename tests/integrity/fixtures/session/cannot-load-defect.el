;;; cannot-load-defect.el --- A feature waited for that cannot be loaded -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; gterm is on the deny list: never loaded, never reported.  A key
;; binds t1fx-absent-command, which autoloads from a library that does
;; not exist (live.el).
(use-package t1fx-library)
(with-eval-after-load 't1fx-absent-feature
  (ignore))
(use-package gterm)
(keymap-set t1fx-clean-map "C-c n" #'t1fx-absent-command)

;;; cannot-load-defect.el ends here
