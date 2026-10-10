;;; duplicate-binding-clean.el --- Fixture: each key bound once per map -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

;; The same key in two maps, and a key of another case.
(global-set-key (kbd "C-c f") #'fixture-find)
(define-key fixture-mode-map (kbd "C-c f") #'fixture-find-here)
(keymap-global-set "C-c F" #'fixture-find-other)

;; A key removed from a map, then bound.
(define-key fixture-mode-map (kbd "RET") nil)
(keymap-set fixture-mode-map "RET" #'fixture-return)
