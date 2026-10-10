;;; mode-map-shadow-defect.el --- Fixture: mode maps over global keys -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(global-set-key (kbd "C-c t") #'fixture-terminal)
(global-set-key (kbd "C-c a") #'fixture-agenda)

;; The same key in Python buffers.
(define-key python-base-mode-map (kbd "C-c t") #'fixture-test-menu)

;; A key under a global command makes it a prefix in this mode.
(use-package fixture
  :bind (:map fixture-mode-map
         ("C-c a b" . fixture-build)))
