;;; lambda-binding-defect.el --- Fixture: lambdas bound to keys -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(define-key fixture-mode-map (kbd "M-<left>")
  (lambda () (interactive) (fixture-left)))

(use-package fixture
  :bind (:map fixture-mode-map
         ("M-<right>" . (lambda () (interactive) (fixture-right)))))
