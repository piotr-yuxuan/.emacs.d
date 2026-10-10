;;; package-key-shadow-clean.el --- Reserved global keys no package keymap hides -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; In t1fx-package-mode-map (live.el), C-c y runs what the self-test's
;; global map runs; C-c P is a prefix keymap over a global prefix
;; keymap; this configuration binds C-c u there itself; nothing binds
;; C-c e globally; <f19> is not a reserved key.  The self-test allows
;; t1fx-greedy-mode-map.
(keymap-global-set "C-c y" #'t1fx-command)
(use-package t1fx-pkg
  :bind-keymap ("C-c P" . t1fx-clean-map))
(keymap-global-set "C-c u" #'t1fx-plain)
(keymap-set t1fx-package-mode-map "C-c u" #'t1fx-command)
(keymap-global-set "<f19> r" #'t1fx-plain)

;;; package-key-shadow-clean.el ends here
