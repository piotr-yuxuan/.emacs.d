;;; l3-journey-06-magit-stage.el --- Journey 6: C-x g stages a hunk and starts a commit -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, journey 6, and U08 of the 085 mutation baseline.  The
;; user commits the file from a shell command, edits two distant
;; lines, saves, opens Magit's status with C-x g, expands the file's
;; diff with <tab>, moves to the second hunk with n, stages it with s
;; and starts a commit with c c: the commit message buffer opens.  The
;; user then cancels with C-c C-k, which keeps the index, and the
;; status buffer shows the first hunk unstaged and the second staged.
;; When s does not stage, the mutation, c c finds nothing staged and
;; asks whether to commit everything.  git is a real process, so the
;; file is L3; the runner gives it a fixture identity and no signing
;; (tests/runner/gitconfig).

;;; Code:

(require 'scenario)

(scenario
 :describe "J-06: C-x g, s stages one hunk, c c starts the commit"
 :given ((:name "text" :file "notes.txt"
          :text "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\neleven\ntwelve\nthirteen\n"
          :point-on "two"))
 :when ((:keys "M-!") (:type "git add -A && git commit -q -m Initial") (:keys "<return>")
        (:type "first-") (:keys "M-> C-p C-p") (:type "second-") (:keys "C-x C-s")
        (:keys "C-x g n <tab> n n") (:sample "hunk") (:keys "s c c") (:idle 3000)
        (:sample "message") (:keys "C-c C-k"))
 :then ((:sampled "hunk" :line "\\`@@ -9,5 \\+9,5 @@")
        (:sampled "message" :buffer "\\`COMMIT_EDITMSG\\'")
        (:buffer-name "\\`magit: ")
        (:buffer-named-matches
         "\\`magit: "
         "^Unstaged changes (1)\nmodified .*notes\\.txt\n@@ -1,5 \\+1,5 @@\n one\n-two\n\\+first-two\n three\n four\n five\n\nStaged changes (1)\nmodified .*notes\\.txt\n@@ -9,5 \\+9,5 @@ eight\n nine\n ten\n eleven\n-twelve\n\\+second-twelve\n thirteen\n")))

(provide 'l3-journey-06-magit-stage)

;;; l3-journey-06-magit-stage.el ends here
