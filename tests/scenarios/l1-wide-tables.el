;;; l1-wide-tables.el --- Wide Org and Markdown tables stay readable -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 092, from review 084 (J5).  C-c v t opens the Org or Markdown
;; table at point in a read-only box: a border around every cell, every
;; cell in full and wrapped to fit the window, sortable columns, and RET
;; back to the cell in the source.  The view opens on its top rule, which
;; belongs to the first row; <down> reaches that row's first line.  In Org, with
;; org-table-header-line-mode turned on, the top line of the window
;; shows the header row once it has scrolled off.
;; Prose next to the tables wraps as before.  None of these needs a
;; graphical frame; where the header labels are drawn is checked in
;; l1-wide-tables-gui.el.
;;
;; tests/fixtures/wide-tables/ holds wide.org and wide.md, the same
;; 300-column table, and tall.org, a table of 120 rows.

;;; Code:

(require 'scenario)

(scenario
 :describe "WT-VIEW-CELLS: C-c v t lists every cell of a wide table in full"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t"))
 :then ((:mode my/table-view-mode)
        (:header-line-matches "Key +│ +Planet +│ +Summary +│ +Atmosphere +│ +Discovery")
        (:tabulated-rows
         (("3" "Earth"
           "The third planet from the Sun, and the only one known to harbour life"
           "Nitrogen and oxygen, with traces of argon, carbon dioxide and water vapour"
           "Known since prehistory; first photographed whole from space by the crew of Apollo 17, on 7 December 1972, as the Blue Marble")
          ("1" "Mercury"
           "The smallest planet, and the closest to the Sun, with a large iron core"
           "Almost none: a thin exosphere of oxygen, sodium, hydrogen, helium and potassium"
           "Known since antiquity; first visited by the Mariner 10 probe in 1974 and 1975, then orbited by MESSENGER from 2011 to 2015")
          ("2" "Venus"
           "The hottest planet, wrapped in thick clouds of sulphuric acid"
           "Dense carbon dioxide, with a surface pressure about ninety times that of Earth"
           "Known since antiquity; Venera 7 made the first soft landing on it in 1970, and sent back data for twenty-three minutes")))))

(scenario
 :describe "WT-VIEW-RETURN: RET goes back to the cell at point in the unchanged source"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t <down> M-<right> M-<right> <return>"))
 :then ((:buffer-name "\\`wide\\.\\(?:org\\|md\\)\\'")
        (:line-at-point "| Earth +|")
        (:after-point-matches "The third planet ")
        (:buffer-text-unchanged)))

(scenario
 :describe "WT-VIEW-RETURN-WRAPPED: RET on a wrapped line of a row goes back to that row"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 ;; The third line of the box is the second line of the first row, Earth,
 ;; whose cells wrap in a window narrower than the table.
 :when ((:keys "C-c v t <down> <down> M-<right> M-<right> <return>"))
 :then ((:line-at-point "| Earth +|")
        (:after-point-matches "The third planet ")))

(scenario
 :describe "WT-VIEW-RETURN-EOL: RET at the end of a line goes back to its last cell"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t <down> C-e <return>"))
 :then ((:line-at-point "| Earth +|")
        (:after-point-matches "Known since prehistory")))

(scenario
 :describe "WT-VIEW-RETURN-SORTED: after S, RET goes back to the row at point, not to its line number"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t S M-<right> M-<right> <return>"))
 :then ((:line-at-point "| Earth +|")
        (:after-point-matches "The third planet")))

(scenario
 :describe "WT-VIEW-SOURCE-GONE: RET after the source buffer is killed says so"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t C-x k") (:type "wide.") (:keys "<return> <return>"))
 :then ((:buffer-absent "\\`wide\\.\\(?:org\\|md\\)\\'")
        (:user-error "\\`No row of a live table at point\\'")))

(scenario
 :describe "WT-NO-HEADER: without a rule after the first row, columns are named $1, $2"
 :given ((:name "org" :file "plain.org" :text "| a -!-| b |\n| c | d |\n")
         (:name "markdown" :file "plain.md" :text "| a -!-| b |\n| c | d |\n"))
 :when ((:keys "C-c v t"))
 :then ((:header-line-matches "\\$1 +│ +\\$2")
        (:tabulated-rows (("a" "b") ("c" "d")))))

(scenario
 :describe "WT-LABELS: an empty or repeated label becomes the column number"
 :given ((:name "org" :file "labels.org"
          :text "| Total | Total |   | Name |\n|---+---+---+---|\n| 1 -!-| 2 | 3 | x |\n")
         (:name "markdown" :file "labels.md"
          :text "| Total | Total | | Name |\n|---|---|---|---|\n| 1 -!-| 2 | 3 | x |\n"))
 :when ((:keys "C-c v t"))
 :then ((:header-line-matches "\\$1 +│ +\\$2 +│ +\\$3 +│ +Name")
        (:tabulated-rows (("1" "2" "3" "x")))))

(scenario
 :describe "WT-MARKDOWN-EDGES: alignment row, dash data row, escaped pipes, ragged rows"
 :given ((:name "markdown" :file "edges.md"
          :text "| Name | Note | Size |
|:-----|:----:|-----:|
| pipe \\| inside | | 3 |
| short -!-|
| no trailing | pipe
| a | b | c | extra |
| - | - | - |
"))
 :when ((:keys "C-c v t"))
 :then ((:header-line-matches "Name +│ +Note +│ +Size +│ +\\$4")
        (:tabulated-rows (("pipe | inside" "" "3" "")
                          ("short" "" "" "")
                          ("no trailing" "pipe" "" "")
                          ("a" "b" "c" "extra")
                          ("-" "-" "-" "")))))

;; Org export hides both cookie rows: ox.el,
;; `org-export-table-row-is-special-p', matches <[lrc]?\([0-9]+\)?>.
(scenario
 :describe "WT-ORG-COOKIES: rows of cookies, <> included, are hidden as Org export hides them"
 :given ((:name "org" :file "cookies.org"
          :text "| a | b |\n|---+---|\n| <l> | <5> |\n| <> |  |\n| 1 -!-| 2 |\n"))
 :when ((:keys "C-c v t"))
 :then ((:tabulated-rows (("1" "2")))))

(scenario
 :describe "WT-ORG-LEADING-RULE: a rule above the header row does not hide it"
 :given ((:name "org" :file "leading.org"
          :text "|---+---|\n| h1 | h2 |\n|---+---|\n| a -!-| b |\n"))
 :when ((:keys "C-c v t"))
 :then ((:header-line-matches "h1 +│ +h2")
        (:tabulated-rows (("a" "b")))))

(scenario
 :describe "WT-ORG-TWO-HEADER-ROWS: with two rows above the rule, every row is data"
 :given ((:name "org" :file "two.org"
          :text "| h1 | h2 |\n| h1b | h2b |\n|---+---|\n| a -!-| b |\n"))
 :when ((:keys "C-c v t"))
 :then ((:header-line-matches "\\$1 +│ +\\$2")
        (:tabulated-rows (("h1" "h2") ("h1b" "h2b") ("a" "b")))))

(scenario
 :describe "WT-ORG-EMPTY-ROW: a row of empty cells is data, not a rule"
 :given ((:name "org" :file "blank.org"
          :text "| h1 | h2 |\n|---+---|\n| a -!-| b |\n|   |   |\n| c | d |\n"))
 :when ((:keys "C-c v t"))
 :then ((:tabulated-rows (("a" "b") ("" "") ("c" "d")))))

(scenario
 :describe "WT-MARKDOWN-INDENTED: a table indented under a list item is found"
 :given ((:name "markdown" :file "indented.md"
          :text "- Planets:\n\n    | h1 | h2 |\n    |----|----|\n    | a -!-| b |\n"))
 :when ((:keys "C-c v t"))
 :then ((:header-line-matches "h1 +│ +h2")
        (:tabulated-rows (("a" "b")))))

(scenario
 :describe "WT-NO-ROW: C-c v t on a table without a cell says so"
 :given ((:name "org rules only" :file "rules.org" :text "|---+---|-!-\n")
         (:name "markdown lone pipe" :file "pipe.md" :text "|-!-\n"))
 :when ((:keys "C-c v t"))
 :then ((:user-error "\\`No row in the table at point\\'")))

(scenario
 :describe "WT-VIEW-SORT-COLUMN: S sorts by the column at point, not by the first"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t M-<right> M-<right> S"))
 :then ((:table-view-column "Planet" ("Venus" "Mercury" "Earth"))))

(scenario
 :describe "WT-VIEW-BORDERS: every cell has a border, from the top rule to the bottom one"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t"))
 :then ((:buffer-matches "\\`┌─+┬─+┬─+┬─+┬─+┐\n│ 3 +│ Earth +│ The third")
        (:buffer-matches "\n├─+┼─+┼─+┼─+┼─+┤\n│ 1 +│ Mercury +│ The smallest")
        (:buffer-matches "\n└─+┴─+┴─+┴─+┴─+┘\n\\'")))

(scenario
 :describe "WT-VIEW-WRAP: a long cell wraps onto more lines of its row, and the box fits the window"
 :given ((:name "org" :project "wide-tables" :entry "wide.org" :point-on "Mercury")
         (:name "markdown" :project "wide-tables" :entry "wide.md" :point-on "Mercury"))
 :when ((:keys "C-c v t"))
 :then ((:table-fits-window)
        (:buffer-matches "│ 1 +│ Mercury +│ The smallest[^\n]*\n│ +│ +│ [^ │]")))

(scenario
 :describe "WT-VIEW-REFIT: g fits the box again to a window that became narrower"
 :given ((:name "org" :file "notes.org"
          :text "| Name | Notes |\n|---+---|\n| a-!- | A long note, long enough to wrap in half of the window but not in all of it. |\n")
         (:name "markdown" :file "notes.md"
          :text "| Name | Notes |\n|---|---|\n| a-!- | A long note, long enough to wrap in half of the window but not in all of it. |\n"))
 :when ((:keys "C-c v t C-x 3 g"))
 :then ((:mode my/table-view-mode)
        (:table-fits-window)))

(scenario
 :describe "WT-VIEW-PERCENT-LABEL: a % in a label shows as is"
 :given ((:name "org" :file "percent.org"
          :text "| Rate % | %-change | Name |\n|---+---+---|\n| 1 -!-| 2 | x |\n")
         (:name "markdown" :file "percent.md"
          :text "| Rate % | %-change | Name |\n|---|---|---|\n| 1 -!-| 2 | x |\n"))
 :when ((:keys "C-c v t"))
 :then ((:header-line-matches "Rate % +│ +%-change +│ +Name")))

(scenario
 :describe "WT-NO-TABLE: C-c v t away from a table says there is none"
 :given ((:name "org prose" :file "prose.org" :text "Some prose.-!-\n\n| a | b |\n")
         (:name "markdown prose" :file "prose.md" :text "Some prose.-!-\n\n| a | b |\n")
         (:name "elisp comment" :file "prose.el" :text ";; | a | b |-!-\n"))
 :when ((:keys "C-c v t"))
 :then ((:user-error "\\`No Org or Markdown table at point\\'")))

(scenario
 :describe "WT-ORG-HEADER: with the mode on, the top line shows the header row once it has scrolled off"
 :given ((:name "tall table" :project "wide-tables" :entry "tall.org" :point-on "row 1 "))
 :when ((:keys "M-x") (:type "org-table-header-line-mode") (:keys "<return> C-v"))
 :then ((:minor-modes-on org-table-header-line-mode)
        (:window-top-line-matches "Index +| Name +| Value")))

;; This pins an upstream limitation listed in the how-to: Org puts the
;; header row on the top line of the window without checking point.
;; If it fails, Org changed: update the Limits of the how-to.  Point
;; ends on row 15 only if C-u 0 C-l put row 15 on the top line and
;; C-u 0 M-r moved point back up to that line.
(scenario
 :describe "WT-ORG-HEADER-AT-POINT: the header row covers the top line, even with point on it"
 :given ((:name "tall table" :project "wide-tables" :entry "tall.org" :point-on "row 15 "))
 :when ((:keys "M-x") (:type "org-table-header-line-mode")
        (:keys "<return> C-u 0 C-l C-n C-n C-n C-u 0 M-r"))
 :then ((:line-at-point "\\`| +15 | row 15 ")
        (:window-top-line-matches "Index +| Name +| Value")))

;; With `org-table-header-line-mode' on, Org 10.0-pre loops forever:
;; the table at the top of the window is one rule up to the end of a
;; buffer without a final newline.  The mode is off by default.
(scenario
 :describe "WT-ORG-TYPE-RULE: typing |- in a new Org file does not hang"
 :given ((:name "new org file" :file "new.org" :text "-!-"))
 :when ((:type "|-"))
 :then ((:minor-modes-off org-table-header-line-mode)
        (:buffer-text "|-")))

(scenario
 :describe "WT-PROSE-MARKDOWN: prose next to a Markdown table still wraps"
 :given ((:name "markdown" :project "wide-tables" :entry "wide.md"))
 :when ((:open))
 :then ((:line-wraps "^The planets of the Solar System")))

(scenario
 :describe "WT-PROSE-ORG: prose typed next to an Org table is still filled"
 :given ((:name "org" :file "notes.org"
          :text "Prose before the table.-!-\n\n| a | b |\n|---+---|\n| 1 | 2 |\n"))
 :when ((:type " It goes on for long enough that auto-fill breaks it at the fill column, \
well before it reaches the end of this rather long sentence about tables."))
 :then ((:buffer-matches "\\`Prose before the table\\.[^\n]*\n[^|\n][^|]*\n\n| a | b |")))


(provide 'l1-wide-tables)

;;; l1-wide-tables.el ends here
