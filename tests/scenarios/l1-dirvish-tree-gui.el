;;; l1-dirvish-tree-gui.el --- dirvish draws one icon per entry and collapsed chains -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Plan 089.  dirvish draws its icons, the collapsed chains and the
;; empty-directory face as overlays, and only on a graphical frame, so
;; these rows are expected skips under EMACS_TEST_GUI=0.  The fixture
;; is described in l1-dirvish-tree.el.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-DIRVISH-ICONS: each entry of a directory shows exactly one icon"
 :given ((:name "file-tree root" :project "file-tree" :entry "."))
 :when ((:open))
 :then ((:dirvish-session)
        (:file-icons 1)))

(scenario
 :describe "BL1-DIRVISH-COLLAPSE: a chain shows its tail on its first line, past a hidden file"
 :given ((:name "src chain" :project "file-tree" :entry "."))
 :when ((:open))
 :then ((:line-display-matches " src$" "src.+main.+java.+com.+example.+App\\.java")
        ;; lib/ holds two files: no chain, only the columns of its details.
        (:line-display-matches " lib$" "lib\\(?: +[0-9].*\\| *\\)$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-OMIT: with omitted files hidden, the chain goes past them"
 :given ((:name "lib" :project "file-tree" :entry "lib"))
 :when ((:keys "C-x M-o"))
 :then ((:line-display-matches " omit$" "omit.+only.+real\\.txt")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-LOOP: a directory holding only a link to itself shows no chain"
 :given ((:name "lib" :project "file-tree" :entry "lib"))
 :when ((:open))
 :then ((:line-display-matches " loop$" "loop\\(?: +[0-9].*\\| *\\)$")))

(scenario
 :describe "BL1-DIRVISH-EMPTY: an empty directory keeps the empty-directory face"
 ;; git keeps no empty directory, so the row makes one with +.
 :given ((:name "lib" :project "file-tree" :entry "lib"))
 :when ((:keys "+") (:type "empty") (:keys "<return>"))
 :then ((:line-overlay-face " empty$" dirvish-collapse-empty-dir-face)))

(scenario
 :describe "BL1-DIRVISH-SELF-LINK-ICONS: links that do not resolve keep every line drawn"
 :given ((:name "loopy" :project "file-tree" :entry "lib/loopy"))
 :when ((:open))
 :then ((:file-icons 1)
        (:line-display-matches " solo$" "solo\\(?: +[0-9].*\\| *\\)$")))

(provide 'l1-dirvish-tree-gui)

;;; l1-dirvish-tree-gui.el ends here
