;;; t1fx-library.el --- A fixture package for the T1 self-tests -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by live.el.  It defines one command, and not the commands
;; that live.el and the foreign-command fixtures pretend it defines.

;;; Code:

(defun t1fx-library-command ()
  "Run the fixture library's only command."
  (interactive))

(provide 't1fx-library)

;;; t1fx-library.el ends here
