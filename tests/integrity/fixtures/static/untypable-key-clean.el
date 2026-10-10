;;; untypable-key-clean.el --- Fixture: keys a keyboard sends -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(global-set-key (kbd "C-S-c") #'fixture-copy)
(global-set-key (kbd "S-<f5>") #'fixture-refresh)
(global-set-key (kbd "s-f") #'fixture-find)
(global-set-key (kbd "M-F") #'fixture-forward)
