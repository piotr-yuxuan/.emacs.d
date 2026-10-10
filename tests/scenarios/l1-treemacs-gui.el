;;; l1-treemacs-gui.el --- Treemacs moves to the project of the selected window -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Plan 089.  Without a graphical frame, Treemacs's project follow
;; leaves the tree where it is, so this row is an expected skip under
;; EMACS_TEST_GUI=0.  l1-treemacs.el describes the fixture.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-TREEMACS-PROJECT-FOLLOW: the tree moves to the project of the current buffer"
 :given ((:name "lib, then notes.txt" :project "file-tree" :entry "notes.txt"))
 ;; C-c F d shows lib/ alone; notes.txt belongs to the fixture's project.
 :when ((:keys "C-c F d") (:type "lib") (:keys "<return> C-x o") (:idle 2500))
 :then ((:file-suffix "file-tree/notes.txt")
        (:window-line-at-point "Treemacs" "notes\\.txt")))

(provide 'l1-treemacs-gui)

;;; l1-treemacs-gui.el ends here
