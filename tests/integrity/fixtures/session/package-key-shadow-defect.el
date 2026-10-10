;;; package-key-shadow-defect.el --- Reserved global keys a package keymap hides -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; t1fx-package-mode-map (live.el) binds C-c q to another command than
;; this global binding, and t1fx-greedy-mode-map binds C-c itself to a
;; command, over every C-c key.  t1fx-child-package-mode-map only
;; inherits C-c q, and is not reported.
(keymap-global-set "C-c q" #'t1fx-plain)
(keymap-global-set "C-c j" #'t1fx-plain)

;;; package-key-shadow-defect.el ends here
