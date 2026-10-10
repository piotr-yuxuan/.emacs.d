;;; l1-wide-tables-gui.el --- Wide-table header labels stay above their cells -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Plan 092, from review 084 (J5).  The header line of the box that
;; C-c v t opens follows horizontal scrolling, sorting and the
;; column-width commands, and each label is drawn where the cells of
;; its column start.  Only redisplay tells where a label is drawn, so
;; these scenarios need a graphical frame; the rest of the plan's
;; scenarios are in l1-wide-tables.el.
;;
;; The runner's frame is 76 columns wide.  The 300-column table of
;; tests/fixtures/wide-tables/ wraps to fit it, so the scenarios that
;; scroll sideways use a table of nine columns, wider than the window
;; even with every column at its narrowest, 10 columns.  The view opens
;; on its top rule; <down> reaches the first line of cells.

;;; Code:

(require 'scenario)

(scenario
 :describe "WT-VIEW-HSCROLL: after C-x <, the label of a column stays above its cells"
 :given ((:name "org" :file "many.org"
          :text "| Aaa | Bbb | Ccc | Ddd | Eee | Fff | Ggg | Hhh | Iii |\n|---+---+---+---+---+---+---+---+---|\n| -!-a1 a1 a1 a1 a1 a1 | b2 b2 b2 b2 b2 b2 | c3 c3 c3 c3 c3 c3 | d4 d4 d4 d4 d4 d4 | e5 e5 e5 e5 e5 e5 | f6 f6 f6 f6 f6 f6 | g7 g7 g7 g7 g7 g7 | h8 h8 h8 h8 h8 h8 | i9 i9 i9 i9 i9 i9 |\n")
         (:name "markdown" :file "many.md"
          :text "| Aaa | Bbb | Ccc | Ddd | Eee | Fff | Ggg | Hhh | Iii |\n|---|---|---|---|---|---|---|---|---|\n| -!-a1 a1 a1 a1 a1 a1 | b2 b2 b2 b2 b2 b2 | c3 c3 c3 c3 c3 c3 | d4 d4 d4 d4 d4 d4 | e5 e5 e5 e5 e5 e5 | f6 f6 f6 f6 f6 f6 | g7 g7 g7 g7 g7 g7 | h8 h8 h8 h8 h8 h8 | i9 i9 i9 i9 i9 i9 |\n"))
 :when ((:keys "C-c v t <down>") (:keys "C-u 4 0 C-x <"))
 :then ((:header-label-above-cell "Ggg" "g7")))

(scenario
 :describe "WT-VIEW-SORT: S sorts by the column at point; labels stay above their cells"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t S"))
 :then ((:table-view-column "Planet" ("Mercury" "Venus" "Earth"))
        (:header-label-above-cell "Planet" "Earth")))

(scenario
 :describe "WT-VIEW-HSCROLL-EDGE: a label cut by the window edge stays above its cells"
 :given ((:name "org" :file "many.org"
          :text "| Aaa | Bbb | Ccc | Ddd | Eee | Fff | Ggg | Hhh | Iii |\n|---+---+---+---+---+---+---+---+---|\n| -!-a1 a1 a1 a1 a1 a1 | b2 b2 b2 b2 b2 b2 | c3 c3 c3 c3 c3 c3 | d4 d4 d4 d4 d4 d4 | e5 e5 e5 e5 e5 e5 | f6 f6 f6 f6 f6 f6 | g7 g7 g7 g7 g7 g7 | h8 h8 h8 h8 h8 h8 | i9 i9 i9 i9 i9 i9 |\n")
         (:name "markdown" :file "many.md"
          :text "| Aaa | Bbb | Ccc | Ddd | Eee | Fff | Ggg | Hhh | Iii |\n|---|---|---|---|---|---|---|---|---|\n| -!-a1 a1 a1 a1 a1 a1 | b2 b2 b2 b2 b2 b2 | c3 c3 c3 c3 c3 c3 | d4 d4 d4 d4 d4 d4 | e5 e5 e5 e5 e5 e5 | f6 f6 f6 f6 f6 f6 | g7 g7 g7 g7 g7 g7 | h8 h8 h8 h8 h8 h8 | i9 i9 i9 i9 i9 i9 |\n"))
 :when ((:keys "C-c v t <down>") (:keys "C-u 8 1 C-x <"))
 :then ((:header-label-above-cell "gg" "7 g7")))

(scenario
 :describe "WT-VIEW-NARROW: after { narrows a column below its label, later labels stay above their cells"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t <down> M-<right> M-<right> C-u 6 6 { <down>"))
 :then ((:header-label-above-cell "Atmosphere" "Nitrogen")))

(scenario
 :describe "WT-VIEW-FOLLOW: after M-<right> scrolls the window, the label stays above its cells"
 :given ((:name "org" :file "many.org"
          :text "| Aaa | Bbb | Ccc | Ddd | Eee | Fff | Ggg | Hhh | Iii |\n|---+---+---+---+---+---+---+---+---|\n| -!-a1 a1 a1 a1 a1 a1 | b2 b2 b2 b2 b2 b2 | c3 c3 c3 c3 c3 c3 | d4 d4 d4 d4 d4 d4 | e5 e5 e5 e5 e5 e5 | f6 f6 f6 f6 f6 f6 | g7 g7 g7 g7 g7 g7 | h8 h8 h8 h8 h8 h8 | i9 i9 i9 i9 i9 i9 |\n")
         (:name "markdown" :file "many.md"
          :text "| Aaa | Bbb | Ccc | Ddd | Eee | Fff | Ggg | Hhh | Iii |\n|---|---|---|---|---|---|---|---|---|\n| -!-a1 a1 a1 a1 a1 a1 | b2 b2 b2 b2 b2 b2 | c3 c3 c3 c3 c3 c3 | d4 d4 d4 d4 d4 d4 | e5 e5 e5 e5 e5 e5 | f6 f6 f6 f6 f6 f6 | g7 g7 g7 g7 g7 g7 | h8 h8 h8 h8 h8 h8 | i9 i9 i9 i9 i9 i9 |\n"))
 :when ((:keys "C-c v t <down> M-<right> M-<right> M-<right> M-<right> M-<right> M-<right> M-<right>"))
 :then ((:header-label-above-cell "Hhh" "h8")))

(scenario
 :describe "WT-VIEW-PERCENT: after a % in a label, later labels stay above their cells"
 :given ((:name "org" :file "percent.org"
          :text "| Rate % | %-change | Name |\n|---+---+---|\n| 1 -!-| 2 | x |\n")
         (:name "markdown" :file "percent.md"
          :text "| Rate % | %-change | Name |\n|---|---|---|\n| 1 -!-| 2 | x |\n"))
 :when ((:keys "C-c v t <down>"))
 :then ((:header-label-above-cell "Name" "x")))

(scenario
 :describe "WT-VIEW-WIDE-LABELS: labels of wide characters keep later labels above their cells"
 :given ((:name "org" :file "wide-labels.org"
          :text "| 名字名字名字 | 城市城市 | Name |\n|---+---+---|\n| 1 -!-| 2 | x |\n"))
 :when ((:keys "C-c v t <down>"))
 :then ((:header-label-above-cell "Name" "x")))

(provide 'l1-wide-tables-gui)

;;; l1-wide-tables-gui.el ends here
