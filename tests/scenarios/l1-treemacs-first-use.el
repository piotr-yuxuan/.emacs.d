;;; l1-treemacs-first-use.el --- Project follow replaces the root F8 asked for -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089.  On the first use of an empty workspace, F8 asks for a
;; root directory.  `treemacs-project-follow-mode' then shows the
;; project of the current buffer instead, once Emacs is idle, so the
;; answer does not last.  This row pins that interaction; the first F8
;; of a session meets an empty workspace, so it has a file of its own.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-TREEMACS-FIRST-USE: the root F8 asks for gives way to the buffer's project"
 :given ((:name "notes" :project "file-tree" :entry "notes.txt"))
 ;; The prompt starts at the fixture root; lib/ holds no notes.txt.
 :when ((:keys "<f8>") (:type "lib") (:keys "<return>") (:sample "answered")
        (:idle 2500))
 :then ((:sampled "answered" :buffer "Treemacs")
        (:buffer-name "Treemacs")
        (:buffer-matches "notes\\.txt")))

(provide 'l1-treemacs-first-use)

;;; l1-treemacs-first-use.el ends here
