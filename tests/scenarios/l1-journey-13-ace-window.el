;;; l1-journey-13-ace-window.el --- Journey 13: M-o selects another window -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 094, journey 13, and U11 of the 085 mutation baseline.  With
;; three windows, M-o (ace-window) labels them with the home-row keys
;; of `aw-keys', left column first, and the user's key selects that
;; window.  The user splits the frame, shows b.txt in the top-left
;; window and c.txt in the bottom-left one, keeps a.txt on the right,
;; goes back to the top-left window, and presses M-o d for the third
;; window, on the right.  Plain `other-window' would select c.txt.
;; With two windows ace-window switches without asking, which is why
;; the journey needs three.  Without labels, the mutation, M-o cannot
;; select a window.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-13: M-o and a window's label select that window"
 :given ((:name "three windows" :file "a.txt" :text "alpha\n"))
 :when ((:keys "C-x 3 C-x 2 C-x C-f") (:type "b.txt")
        (:keys "<return> C-x o C-x C-f") (:type "c.txt")
        (:keys "<return> C-x o C-x o") (:sample "start")
        (:keys "M-o d"))
 :then ((:sampled "start" :buffer "\\`b\\.txt\\'")
        (:windows-count 3)
        (:buffer-name "\\`a\\.txt\\'")))

(provide 'l1-journey-13-ace-window)

;;; l1-journey-13-ace-window.el ends here
