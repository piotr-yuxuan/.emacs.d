;;; foreign-command-defect.el --- :commands entries the package does not define -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; lib/t1fx-library.el defines t1fx-library-command only; `ignore'
;; comes from another directory.
(use-package t1fx-library
  :commands (t1fx-library-command t1fx-library-absent ignore))

;;; foreign-command-defect.el ends here
