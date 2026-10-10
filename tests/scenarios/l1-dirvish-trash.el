;;; l1-dirvish-trash.el --- Deleting from dirvish moves the file to the trash -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089, U13 of the 085 mutation baseline.  Emacs moves deleted
;; files to the freedesktop trash under XDG_DATA_HOME.  This file sets
;; that variable, for its own session, to a fresh directory of the
;; session's temporary directory, which the runner removes with the
;; session: the user's trash is never touched.

;;; Code:

(require 'scenario)

(setenv "XDG_DATA_HOME" (make-temp-file "xdg-data-" t))

(scenario
 :describe "BL1-DIRVISH-TRASH: D on a file moves it to the trash"
 :given ((:name "notes" :project "file-tree" :entry "." :point-on " notes.txt"))
 :when ((:keys "D y"))
 :then ((:trashed "notes.txt")))

(provide 'l1-dirvish-trash)

;;; l1-dirvish-trash.el ends here
