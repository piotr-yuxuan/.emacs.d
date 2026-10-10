;;; protected-key-clean.el --- Fixture: protected keys kept -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

;; The allowed overrides.
(global-set-key (kbd "C-x f") #'consult-recent-file)
(bind-key* "C-h C-d" #'eldoc-box-help-at-point)

;; A mode map may use a protected key in its own buffers.
(define-key fixture-mode-map (kbd "C-<left>") #'fixture-barf)

;; Keys next to protected ones.
(global-set-key (kbd "C-x t B") #'fixture-bookmark)
(global-set-key (kbd "C-<up>") #'fixture-up)
(global-set-key (kbd "M-9") #'fixture-nine)
