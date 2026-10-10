;;; mu4e-binding-clean.el --- Keys of mu4e maps that run no mu4e command -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; The check is about the command a key runs, not the map it is in.
(define-key mu4e-headers-mode-map (kbd "C-c c") #'t1fx-command)
(define-key mu4e-main-mode-map (kbd "q") nil)
(keymap-set t1fx-clean-map "C-c m" #'t1fx-mail)

;;; mu4e-binding-clean.el ends here
