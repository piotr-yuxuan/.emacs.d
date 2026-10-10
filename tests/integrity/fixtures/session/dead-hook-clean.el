;;; dead-hook-clean.el --- Functions added to hooks that run -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; The self-test remaps t1fx-parent-mode to t1fx-child-mode, which
;; derives from it.  It remaps t1fx-styles-mode away, but not
;; t1fx-scss-mode, which derives from it and opens .t1fx-scss files, so
;; their buffers run t1fx-styles-mode-hook.  gterm is never loaded by
;; the checks, so its hooks are not checked; auto-save-hook is not a
;; mode hook.  The hook of the alias t1fx-hooked-alias-mode is an alias
;; of the hook its mode runs.
(add-hook 't1fx-child-mode-hook #'ignore)
(add-hook 't1fx-styles-mode-hook #'ignore)
(add-hook 't1fx-parent-mode-hook #'ignore)
(add-hook 'gterm-mode-hook #'ignore)
(add-hook 'auto-save-hook #'ignore)
(add-hook 't1fx-hooked-alias-mode-hook #'ignore)
(use-package t1fx-pkg
  :hook ((t1fx-other-mode t1fx-child-mode) . ignore))

;;; dead-hook-clean.el ends here
