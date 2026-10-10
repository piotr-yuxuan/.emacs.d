;;; outcomes/tables-test.el --- Self-tests of the table outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; Each outcome must fail on the plausible wrong view: a cell the list
;; truncated or shifted into the next column, a wrapped row read as
;; several rows, a line wider than the window, a top line that shows
;; the buffer text instead of what covers it, a truncated line taken
;; for a wrapped one.  `:header-label-above-cell' reads what redisplay
;; drew, so its self-test is in tests/scenarios/l1-table-outcomes-gui.el.

;;; Code:

(require 'buttercup)
(require 'scenario/outcome)
(require 'outcomes)

(defun outcomes-tables-test--verdict (clause)
  "Return the verdict of CLAUSE in the current buffer."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause nil))

(describe "Table outcomes"

  (it ":tabulated-rows reads each column of each line, without padding"
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format [("A" 5 t) ("B" 3 t) ("C" 4 t)]
            tabulated-list-entries '((1 ["one" "" "x y"]) (2 ["three" "b" ""])))
      (tabulated-list-print)
      (expect (outcomes-tables-test--verdict
               '(:tabulated-rows (("one" "" "x y") ("three" "b" ""))))
              :to-be nil)
      ;; The empty cell dropped, so that "x y" moves into column B.
      (expect (outcomes-tables-test--verdict
               '(:tabulated-rows (("one" "x y" "") ("three" "b" ""))))
              :to-equal "the list shows ((\"one\" \"\" \"x y\") (\"three\" \"b\" \"\"))")
      (expect (outcomes-tables-test--verdict '(:tabulated-rows (("one" "" "x y"))))
              :to-be-truthy)))

  (it ":tabulated-rows fails on a cell the list truncated"
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format [("A" 3 t) ("B" 3 t)]
            tabulated-list-entries '((1 ["three" "x"])))
      (tabulated-list-print)
      ;; The buffer still holds "three"; a `display' property elides "ree".
      (expect (outcomes-tables-test--verdict '(:tabulated-rows (("three" "x"))))
              :to-equal (format "the list shows ((\"th%s\" \"x\"))"
                                (truncate-string-ellipsis)))))

  (it ":tabulated-rows joins the lines of a wrapped row, inside its borders"
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format [("A" 3 t) ("B" 5 t)])
      ;; Row 1 wraps its B cell over two lines; a closing rule has no row.
      (let ((inhibit-read-only t))
        (dolist (line '((nil "┌─────" "┬───────┐")
                        (1 "│ one " "│ two   │")
                        (1 "│     " "│ three │")
                        (2 "│ x   " "│ y     │")
                        (nil "└─────" "┴───────┘")))
          (let ((start (point)))
            (insert (propertize (nth 1 line) 'tabulated-list-column-name "A")
                    (propertize (nth 2 line) 'tabulated-list-column-name "B")
                    "\n")
            (when (car line)
              (put-text-property start (point) 'tabulated-list-id (car line))))))
      (expect (outcomes-tables-test--verdict
               '(:tabulated-rows (("one" "two three") ("x" "y"))))
              :to-be nil)
      ;; One line read as one row: the continuation line becomes a row.
      (expect (outcomes-tables-test--verdict
               '(:tabulated-rows (("one" "two") ("" "three") ("x" "y"))))
              :to-be-truthy)
      (expect (outcomes-tables-test--verdict '(:table-view-column "B" ("two three" "y")))
              :to-be nil)
      (expect (outcomes-tables-test--verdict '(:table-view-column "B" ("y" "two three")))
              :to-equal "the column shows (\"two three\" \"y\")")
      (expect (outcomes-tables-test--verdict '(:table-view-column "C" ("y")))
              :to-equal "no column is named \"C\"")))

  (it ":table-fits-window measures the widest line against the window"
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert (make-string (window-body-width) ?─) "\nshort\n")
        (expect (outcomes-tables-test--verdict '(:table-fits-window)) :to-be nil)
        (insert (make-string (1+ (window-body-width)) ?─) "\n")
        (expect (outcomes-tables-test--verdict '(:table-fits-window))
                :to-equal (format "a line is %d columns wide, in a window of %d"
                                  (1+ (window-body-width)) (window-body-width))))))

  (it ":window-top-line-matches reads the top line as displayed"
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert "| 7 | row 7 |\n| 8 | row 8 |\n")
        (set-window-start (selected-window) (point-min))
        (expect (outcomes-tables-test--verdict '(:window-top-line-matches "row 7"))
                :to-be nil)
        (overlay-put (make-overlay 1 14) 'display "| Index | Name |")
        (expect (outcomes-tables-test--verdict '(:window-top-line-matches "Index"))
                :to-be nil)
        (expect (outcomes-tables-test--verdict '(:window-top-line-matches "row 7"))
                :to-equal "the top line shows \"| Index | Name |\""))))

  (it ":after-point-matches reads from point, not from the line start"
    (with-temp-buffer
      (insert "| 1 | The smallest planet |")
      (goto-char 7)
      (expect (outcomes-tables-test--verdict '(:after-point-matches "The smallest"))
              :to-be nil)
      (expect (outcomes-tables-test--verdict '(:after-point-matches "| 1"))
              :to-be-truthy)))

  (it ":line-wraps tells a wrapped line from a truncated one"
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert (make-string (* 2 (window-width)) ?a) "\nshort\n")
        (setq truncate-lines nil)
        (expect (outcomes-tables-test--verdict '(:line-wraps "^a")) :to-be nil)
        (expect (outcomes-tables-test--verdict '(:line-wraps "^short"))
                :to-equal "the line takes one screen line")
        (setq truncate-lines t)
        (expect (outcomes-tables-test--verdict '(:line-wraps "^a"))
                :to-equal "the line takes one screen line")))))

(provide 'outcomes/tables-test)

;;; outcomes/tables-test.el ends here
