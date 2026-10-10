;;; variables-defect.el --- Settings of undeclared and obsolete variables -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; t1fx-ghost-variable and t1fx-local-ghost are declared nowhere;
;; t1fx-old-variable is an obsolete alias (live.el).  t1fx-lazy-option
;; is declared by lib/t1fx-lazy.el, which only the check loads.  The
;; variables of a package not loaded, and of code for another platform,
;; are not checked: that code is the body of a `when' or `unless' whose
;; test of system-type skips it, whatever the unbound t1fx-flag holds.
;; t1fx-twice-ghost is also set outside such code, and t1fx-flag-ghost
;; under a test that depends on t1fx-flag: both are checked.
(setq t1fx-declared 1
      t1fx-ghost-variable 2
      t1fx-old-variable 3
      t1fx-lazy-option t)
(add-hook 't1fx-child-mode-hook (lambda () (setq-local t1fx-local-ghost 4)))
(use-package t1fx-skipped
  :custom (t1fx-skipped-option 1))
(when (eq system-type 't1fx-elsewhere)
  (setq t1fx-elsewhere-variable 1
        t1fx-twice-ghost 1))
(unless (not (eq 't1fx-elsewhere system-type))
  (setq t1fx-never-variable 1))
(when (and t1fx-flag (memq system-type '(t1fx-elsewhere t1fx-other)))
  (setq t1fx-flagged-variable 1))
(when (or t1fx-flag (eq system-type 't1fx-elsewhere))
  (setq t1fx-flag-ghost 1))
(setq t1fx-twice-ghost 2)

;;; variables-defect.el ends here
