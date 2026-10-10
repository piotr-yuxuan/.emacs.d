;;; lambda-binding-clean.el --- Fixture: named commands bound to keys -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(define-key fixture-mode-map (kbd "M-<left>") #'fixture-left)
(keymap-set fixture-mode-map "M-<up>" 'fixture-up)

(use-package fixture
  :bind (:map fixture-mode-map
         ("M-<right>" . fixture-right))
  ;; A lambda on a hook is not a binding.
  :hook (fixture-mode . (lambda () (setq-local fixture-width 80))))
