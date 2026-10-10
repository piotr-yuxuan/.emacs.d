;;; variables-clean.el --- Settings of declared, current variables -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; t1fx-current is also set for another platform, and t1fx-declared
;; under a test that is never evaluated: t1fx-flag is unbound.
(setq t1fx-declared 1)
(setq-default t1fx-current 2)
(when (eq system-type system-type)
  (setq t1fx-declared 3))
(when (eq system-type 't1fx-elsewhere)
  (setq t1fx-current 4))
(when (and t1fx-flag (eq system-type 't1fx-elsewhere))
  (setq t1fx-declared 5))

;;; variables-clean.el ends here
