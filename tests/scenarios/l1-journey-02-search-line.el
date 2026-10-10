;;; l1-journey-02-search-line.el --- Journey 2: C-s searches lines with consult -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 094, journey 2, and U02 of the 085 mutation baseline.  C-s is
;; consult-line (decision D11): the minibuffer prompts "Go to line"
;; and lists every line matching the typed text, from point on;
;; <return> jumps to the first.  With C-s bound to isearch-forward,
;; the mutation, the same keys also move point, but no minibuffer
;; lists the lines.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-02: C-s lists the matching lines and <return> jumps to the first"
 :given ((:name "text" :file "notes.txt"
          :text "-!-alpha\nfirst needle here\nbeta\nsecond needle there\n"))
 :when ((:keys "C-s") (:type "needle") (:sample "candidates") (:keys "<return>"))
 :then ((:sampled "candidates" :minibuffer
                  "Go to line: needle\\(?:.\\|\n\\)*first needle here\\(?:.\\|\n\\)*second needle there")
        (:line-at-point "\\`first needle here\\'")))

(provide 'l1-journey-02-search-line)

;;; l1-journey-02-search-line.el ends here
