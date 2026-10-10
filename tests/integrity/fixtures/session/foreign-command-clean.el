;;; foreign-command-clean.el --- :commands entries the package defines -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; gterm is never loaded by the checks, so it is not checked.
(use-package t1fx-library
  :commands t1fx-library-command)
(use-package gterm
  :commands gterm-absent)

;;; foreign-command-clean.el ends here
