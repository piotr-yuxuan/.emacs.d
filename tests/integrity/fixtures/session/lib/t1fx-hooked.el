;;; t1fx-hooked.el --- A fixture mode library only the force-load loads -*- lexical-binding: t -*-

;;; Commentary:
;; live.el autoloads `t1fx-hooked-mode' from here, and a fixture adds
;; to its hook: the force-load must load it, as it loads css-mode.el
;; for css-mode-hook, so that the modes derived from it are defined.

;;; Code:

(define-derived-mode t1fx-hooked-mode fundamental-mode "Hooked"
  "A fixture major mode whose hook the configuration adds to.")

(provide 't1fx-hooked)

;;; t1fx-hooked.el ends here
