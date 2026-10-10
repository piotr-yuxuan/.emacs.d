;;; l3-journey-07-diff-hl-hunk.el --- Journey 7: C-c g n reaches an edited hunk -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, journey 7, and U07 of the 085 mutation baseline.  The
;; user commits the file from a shell command, reverts the buffer so
;; that Emacs sees the file as tracked (as after any commit made
;; outside Emacs), edits a line further down, saves, goes back to the
;; top, and C-c g n (diff-hl-next-hunk) moves to the edited line.
;; Without diff-hl in the buffer, the mutation, there is no hunk to
;; reach.  git is a real process, so the file is L3; the runner gives
;; it a fixture identity and no signing (tests/runner/gitconfig).

;;; Code:

(require 'scenario)

(scenario
 :describe "J-07: after an edit in a tracked file, C-c g n reaches the hunk"
 :given ((:name "elisp" :file "notes.el"
          :text "(defun alpha ()\n  1)\n\n(defun beta ()\n  2)\n\n(defun gamma ()\n  3)\n"
          :point-on "gamma ()")
         (:name "text" :file "notes.txt"
          :text "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta\n"
          :point-on "epsilon"))
 :when ((:keys "M-!") (:type "git add -A && git commit -q -m Initial") (:keys "<return>")
        (:keys "C-x x g") (:type "changed-") (:keys "C-x C-s M-<") (:idle 1000)
        (:keys "C-c g n"))
 :then ((:line-at-point "\\`\\(?:(defun \\)?changed-\\(?:gamma ()\\|epsilon\\)\\'")))

(provide 'l3-journey-07-diff-hl-hunk)

;;; l3-journey-07-diff-hl-hunk.el ends here
