;;; untypable-key-defect.el --- Fixture: keys no keyboard sends -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.  Shift and a
;; letter arrive as the uppercase letter.

(global-set-key (kbd "S-f") #'isearch-forward)
(keymap-set fixture-mode-map "M-S-g" #'fixture-go)
