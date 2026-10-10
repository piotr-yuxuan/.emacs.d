;;; mu4e-binding-defect.el --- Keys bound to mu4e commands -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

(use-package mu4e
  :bind ("C-c m" . mu4e))
(keymap-set t1fx-clean-map "C-c u" #'mu4e-update-mail-and-index)
(keymap-set t1fx-clean-map "C-c a" #'t1fx-command)

;;; mu4e-binding-defect.el ends here
