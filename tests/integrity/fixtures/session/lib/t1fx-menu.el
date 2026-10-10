;;; t1fx-menu.el --- A fixture library only the force-load loads -*- lexical-binding: t -*-

;;; Commentary:
;; live.el autoloads `t1fx-menu-command' from here and loads nothing
;; else: the force-load must load it for the command a key binds.

;;; Code:

(defun t1fx-menu-command ()
  "Open the fixture menu."
  (interactive))

(provide 't1fx-menu)

;;; t1fx-menu.el ends here
