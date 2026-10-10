;;; l1-journey-12-save-whitespace.el --- Journey 12: saving strips only touched lines -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 094, journey 12, and K05 of the 085 mutation baseline.  The
;; user types a line ending with spaces under a line that already ends
;; with spaces, leaves the typed line and saves.  ws-butler strips the
;; whitespace of the line the user touched and leaves the untouched
;; line alone, so the diff of a save shows only the user's edit.  A
;; global `delete-trailing-whitespace' on save, the mutation, strips
;; both lines.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-12: saving strips trailing whitespace on touched lines only"
 :given ((:name "text" :file "notes.txt" :text "untouched line   \n-!-\n")
         (:name "elisp" :file "notes.el" :text "untouched line   \n-!-\n"))
 :when ((:type "typed line   ") (:keys "C-<home> C-x C-s"))
 :then ((:buffer-text "untouched line   \ntyped line\n")))

(provide 'l1-journey-12-save-whitespace)

;;; l1-journey-12-save-whitespace.el ends here
