;;; dead-hook-defect.el --- Functions added to mode hooks that never run -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; t1fx-absent-mode and t1fx-gone-mode are defined nowhere; the
;; self-test remaps t1fx-old-mode to t1fx-parent-mode, which does not
;; derive from it, and t1fx-legacy-mode to t1fx-ts-mode, which names it
;; an extra parent only.  Of the modes derived from t1fx-legacy-mode,
;; t1fx-legacy-child-mode is remapped to t1fx-ts-mode too, and
;; t1fx-slides-mode opens no file.  t1fx-alias-mode is an alias (live.el).
(add-hook 't1fx-absent-mode-hook #'ignore)
(use-package t1fx-pkg
  :hook (t1fx-gone-mode . ignore))
(dolist (hook '(t1fx-old-mode-hook))
  (add-hook hook #'ignore))
(add-hook 't1fx-alias-mode-hook #'ignore)
(add-hook 't1fx-legacy-mode-hook #'ignore)

;;; dead-hook-defect.el ends here
