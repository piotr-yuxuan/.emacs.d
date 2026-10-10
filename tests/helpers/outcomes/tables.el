;;; outcomes/tables.el --- Outcomes about tables and wrapped lines -*- lexical-binding: t -*-

;;; Commentary:
;; What the user sees of a table: in a Tabulated List view, the cells
;; of each row, on one line or wrapped over several, the header line
;; above them, and whether the table fits the window; in the source
;; buffer, the top line of the window, the text at point, and whether
;; a line wraps.  See `scenario/outcome.el' for the contract.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'scenario/outcome)
(require 'scenario/display)

(defconst my/scenario-table-border "[─│┌┐└┘├┤┬┴┼ \t]+"
  "Regexp of the box-drawing borders and padding around a cell.")

(defun my/scenario-table-rows ()
  "Return the rows the Tabulated List of the current buffer shows.
A row is the list of its cells, one per column of
`tabulated-list-format'.  A row may span several lines, all with its
`tabulated-list-id'; lines without one, such as a closing rule, belong
to no row.  A cell is the text it shows on each of the row's lines, as
displayed, without its borders and padding, joined by spaces; a column
a line does not show is empty.  See `my/scenario-displayed-text' for
what displayed means."
  (let ((names (mapcar #'car tabulated-list-format))
        (rows nil)
        (last-id nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((id (get-text-property (point) 'tabulated-list-id))
              (cells (make-list (length names) ""))
              (pos (point)))
          (while (< pos (pos-eol))
            (let ((next (next-single-property-change pos 'tabulated-list-column-name
                                                     nil (pos-eol)))
                  (name (get-text-property pos 'tabulated-list-column-name)))
              (when name
                (setf (nth (seq-position names name) cells)
                      (string-trim (my/scenario-displayed-text pos next)
                                   my/scenario-table-border my/scenario-table-border)))
              (setq pos next)))
          (cond ((not id))
                ((and rows (equal id last-id))
                 (setcar rows (cl-mapcar (lambda (shown more) (string-trim (concat shown " " more)))
                                         (car rows) cells)))
                (t (push cells rows)))
          (setq last-id id))
        (forward-line)))
    (nreverse rows)))

(my/scenario-define-outcome :tabulated-rows (_context rows)
  "The Tabulated List of the current buffer shows ROWS.
Each row is a list of strings, one per column of
`tabulated-list-format': the cell as `my/scenario-table-rows' reads it,
whether the list shows it on one line or wraps it over several.
Tabulated List truncates a cell with a `display' property, so the
displayed text is what shows it."
  (let ((shown (my/scenario-table-rows)))
    (unless (equal shown rows)
      (format "the list shows %S" shown))))

(my/scenario-define-outcome :table-view-column (_context name values)
  "The column NAME of the Tabulated List shows VALUES, from top to bottom.
Each value is a cell as `my/scenario-table-rows' reads it."
  (let ((index (seq-position (mapcar #'car tabulated-list-format) name)))
    (if (not index)
        (format "no column is named %S" name)
      (let ((shown (mapcar (lambda (row) (nth index row)) (my/scenario-table-rows))))
        (unless (equal shown values)
          (format "the column shows %S" shown))))))

(my/scenario-define-outcome :table-fits-window (_context)
  "No line of the current buffer is wider than the selected window.
A line's width is that of its text as displayed, against the width of
the window's text area.  A space aligned with `:align-to' counts as one
column; a table's rules, which have none, still show its full width."
  (let ((width (window-body-width))
        (widest 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq widest (max widest (string-width (my/scenario-displayed-text (pos-bol) (pos-eol)))))
        (forward-line)))
    (when (> widest width)
      (format "a line is %d columns wide, in a window of %d" widest width))))

(my/scenario-define-outcome :header-label-above-cell (_context label cell)
  "The header line shows LABEL where CELL starts on the line at point.
Both columns are read from what redisplay drew in the selected
window, after horizontal scrolling: the first occurrence of LABEL in
the header line, and the first occurrence of CELL on the line.  Only
a graphical frame redisplays, so a file using this outcome declares
`scenario-display: graphic'."
  (let* ((window (selected-window))
         (width (frame-char-width))
         (start (save-excursion
                  (beginning-of-line)
                  (and (search-forward cell (pos-eol) t) (match-beginning 0))))
         (x (car (posn-x-y (and start (posn-at-point start window)))))
         ;; The header line, one character per column of the text area.
         (header (mapconcat
                  (lambda (column)
                    (let ((shown (posn-string (posn-at-x-y (+ (* column width) (/ width 2))
                                                           (/ (window-header-line-height window) 2)
                                                           window))))
                      (if (and shown (< (cdr shown) (length (car shown))))
                          (string (aref (car shown) (cdr shown)))
                        " ")))
                  (number-sequence 0 (1- (window-body-width window)))
                  "")))
    (cond ((not (display-graphic-p))
           "no graphical frame shows the window")
          ((not (and x (<= 0 x)))
           (format "the line at point does not show %S" cell))
          ((not (eql (string-search label header) (/ x width)))
           (format "the header line shows %S, and %S starts at column %d"
                   header cell (/ x width))))))

(my/scenario-define-outcome :window-top-line-matches (_context regexp)
  "The top line of the selected window displays as text matching REGEXP.
See `my/scenario-displayed-text' for what displayed means."
  (save-excursion
    (goto-char (window-start))
    (let ((case-fold-search nil)
          (shown (my/scenario-displayed-text (pos-bol) (pos-eol))))
      (unless (string-match-p regexp shown)
        (format "the top line shows %S" shown)))))

(my/scenario-define-outcome :after-point-matches (_context regexp)
  "The text that starts at point matches REGEXP."
  (let ((case-fold-search nil))
    (unless (looking-at-p regexp)
      (format "the text at point is %S"
              (buffer-substring-no-properties (point) (min (point-max) (+ (point) 40)))))))

(my/scenario-define-outcome :line-wraps (_context regexp)
  "The first line matching REGEXP takes more than one screen line.
Soft wrapping, as `visual-line-mode' does it, wraps a long line;
truncation, as with `truncate-lines', does not."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (if (not (re-search-forward regexp nil t))
          (format "no line matches %S" regexp)
        (let ((end (pos-eol)))
          (goto-char (pos-bol))
          ;; The second screen line of a wrapped line starts before its end.
          (unless (and (= (vertical-motion 1 (selected-window)) 1) (< (point) end))
            "the line takes one screen line"))))))

(provide 'outcomes/tables)

;;; outcomes/tables.el ends here
