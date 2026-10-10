;;; global-mode-off-clean.el --- Global modes turned on, and modes that are not global or not at startup -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

(t1fx-on-mode 1)
(t1fx-local-mode 1)
(t1fx-off-mode -1)
(add-hook 'prog-mode-hook #'t1fx-off-mode)
(use-package t1fx-pkg
  :hook (emacs-startup . t1fx-on-mode))

;;; global-mode-off-clean.el ends here
