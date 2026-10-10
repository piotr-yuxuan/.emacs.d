;;; l1-treemacs.el --- Treemacs collapses chains, follows the current file, has its keys -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089.  Each row opens the tree with C-c F d, accepting the
;; current directory: that shows the fixture as the only project,
;; whatever an earlier row left in the workspace, and never meets the
;; first-use prompt of an empty workspace (l1-window-fkey-dispatch.el
;; covers that one, with F8).  The rows open text files only, which
;; start no language server.
;;
;; The fixture file-tree lists lib/ (two files), src/, test/ and
;; notes.txt.  test/ heads the chain test/java/com/example/app, four
;; directories below test/ with nothing else in them; Treemacs sorts
;; directories first, so the chain is the fourth line of the tree.
;; (src/main holds a hidden .gitkeep, which Treemacs shows, so that
;; chain stops there.)

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-TREEMACS-COLLAPSE: a collapsed chain is one node that expands to its last directory"
 :given ((:name "java package chain" :project "file-tree" :entry "notes.txt"))
 :when ((:keys "C-c F d <return> M-< n n n") (:sample "chain") (:keys "<tab>"))
 :then ((:sampled "chain" :line "test/java/com/example/app")
        (:buffer-name "Treemacs")
        (:buffer-matches "AppTest\\.java")))

(scenario
 :describe "BL1-TREEMACS-FOLLOW: after a buffer switch, the tree selects the current file"
 :given ((:name "notes then two.txt" :project "file-tree" :entry "notes.txt"))
 :when ((:keys "C-c F d <return> C-x o C-x C-f")
        (:type "lib/two.txt")
        (:keys "<return>")
        (:idle 1000))
 :then ((:file-suffix "file-tree/lib/two.txt")
        (:window-line-at-point "Treemacs" "two\\.txt")))

(scenario
 :describe "BL1-TREEMACS-SELECT: C-c F s selects the tree window"
 :given ((:name "notes" :project "file-tree" :entry "notes.txt"))
 :when ((:keys "C-c F d <return> C-x o") (:sample "file") (:keys "C-c F s"))
 :then ((:sampled "file" :buffer "\\`notes\\.txt\\'")
        (:windows-count 2)
        (:buffer-name "Treemacs")))

(provide 'l1-treemacs)

;;; l1-treemacs.el ends here
