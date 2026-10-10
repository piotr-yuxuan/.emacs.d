;;; l1-journey-01-recent-file.el --- Journey 1: C-x b reaches a recent file -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 094, journey 1.  The user opens a file, closes its buffer, and
;; later comes back to it with C-x b and a fuzzy pattern: orderless
;; matches the words of the pattern in any order, and consult-buffer
;; lists the recent files that no buffer visits any more.  Without
;; recentf, the mutation, the pattern matches nothing and <return>
;; creates an empty buffer of that name instead.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-01: C-x b with a fuzzy pattern switches to a recent file"
 :given ((:name "closed file" :project "journey-project" :entry "README.txt"))
 :when ((:keys "C-x C-f") (:type "docs/release-checklist.txt") (:keys "<return>")
        (:keys "C-x k <return>")
        (:keys "C-x b") (:type "checklist rel") (:sample "pattern")
        (:keys "<return>"))
 :then ((:sampled "pattern" :minibuffer "release-checklist\\.txt")
        (:file-suffix "/journey-project/docs/release-checklist.txt")
        (:buffer-matches "\\`Tag the release\\.\n")))

(provide 'l1-journey-01-recent-file)

;;; l1-journey-01-recent-file.el ends here
