;;; cannot-load-clean.el --- Features that load -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; t1fx-menu-command autoloads from lib/t1fx-menu.el, and the hooked
;; t1fx-hooked-mode from lib/t1fx-hooked.el, which the force-load loads;
;; t1fx-ghost autoloads from a library already loaded, which is left
;; alone (live.el).
(use-package t1fx-library)
(with-eval-after-load 't1fx-library
  (ignore))
(keymap-set t1fx-clean-map "C-c m" #'t1fx-menu-command)
(keymap-set t1fx-dangling-map "C-c d" 't1fx-ghost)
(add-hook 't1fx-hooked-mode-hook #'ignore)

;;; cannot-load-clean.el ends here
