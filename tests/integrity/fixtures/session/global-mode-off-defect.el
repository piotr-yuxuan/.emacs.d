;;; global-mode-off-defect.el --- A global mode turned on at startup that is off -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

(t1fx-on-mode 1)
(add-hook 'emacs-startup-hook #'t1fx-off-mode)

;;; global-mode-off-defect.el ends here
