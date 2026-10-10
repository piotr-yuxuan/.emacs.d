;;; l3-journey-03-project-search.el --- Journey 3: C-c s r searches the project -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, journey 3.  C-c s r is consult-ripgrep in the current
;; project: the user types a word, waits for ripgrep's matches to
;; appear in the minibuffer, and <return> opens the file of the first
;; match on its line.  ripgrep is a real process, so the file is L3.
;; It is a requirement, not an optional tool: without rg on
;; `exec-path', the mutation, the journey fails.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-03: C-c s r lists the project's matches and <return> opens one"
 :given ((:name "search" :project "journey-project" :entry "README.txt"))
 :when ((:keys "C-c s r") (:type "frobnicate") (:idle 2000) (:sample "matches")
        (:keys "<return>"))
 :then ((:sampled "matches" :minibuffer
                  "Ripgrep.*frobnicate\\(?:.\\|\n\\)*src/greeting\\.el *\n4: .*look for frobnicate")
        (:file-suffix "/journey-project/src/greeting.el")
        (:line-at-point "journeys look for frobnicate")))

(provide 'l3-journey-03-project-search)

;;; l3-journey-03-project-search.el ends here
