;;; l3-pty-eat.el --- The terminal parity matrix: eat -*- lexical-binding: t -*-
;; scenario-tier: l3
;; scenario-display: graphic

;;; Commentary:
;; The eat row of the terminal parity matrix of plan 091.  Each
;; scenario is one capability, with the same keys and outcomes in
;; l3-pty-eat.el, l3-pty-mistty.el and l3-pty-gterm.el, except
;; how the terminal opens, and how its scrollback is searched: from
;; eat-emacs-mode (C-c C-e).
;;
;; The shell is zsh with the fixture startup files of
;; tests/fixtures/pty-zdotdir/ (see tests/pty/pty-session.el), in a
;; copy of tests/fixtures/pty-project/.
;;
;; eat's one known gap is timing: with its default latencies, Emacs
;; stalls under bulk output.  That scenario, timed against the wall
;; clock, is defined only when the environment variable
;; PTY_MATRIX_GAPS is set, as are the gaps of the other two files:
;; PTY_MATRIX_GAPS=1 tests/run full l3-pty, on an idle machine.

;;; Code:

(require 'scenario)
(require 'pty-session (expand-file-name "../pty/pty-session.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))

(scenario
 :describe "PTY eat: opens at the project root, prompt ready"
 :given ((:name "eat" :project "pty-project" :entry "sub/pty-unique-file.txt"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>"))
 :then ((:terminal-shows "^pty-project ready> ")
        (:terminal-directory "../")))

(scenario
 :describe "PTY eat: tracks the directory after cd"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "cd sub") (:keys "<return>"))
 :then ((:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY eat: C-r picks a command from the history with fzf"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:keys "C-r") (:idle 1000) (:type "history-pick") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^history-pick$")))

(scenario
 :describe "PTY eat: C-t inserts a file path with fzf"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "echo ") (:keys "C-t") (:idle 1000) (:type "pty-unique") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^sub/pty-unique-file\\.txt$")))

(scenario
 :describe "PTY eat: M-c changes to a directory with fzf"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:keys "M-c") (:idle 1000) (:type "sub") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY eat: M-<left> and M-<right> move by words"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "echo alpha beta") (:idle 300)
        (:keys "M-<left>") (:idle 300) (:keys "M-<left>") (:idle 300)
        (:keys "M-<right>") (:idle 300) (:type "Y") (:idle 300) (:keys "<return>"))
 :then ((:terminal-shows "^alpha Ybeta$")))

(scenario
 :describe "PTY eat: M-<backspace> and C-<backspace> delete words"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "echo alpha beta gamma") (:idle 300)
        (:keys "M-<backspace>") (:idle 300) (:keys "C-<backspace>") (:idle 300)
        (:type "delta") (:idle 300) (:keys "<return>"))
 :then ((:terminal-shows "^alpha delta$")))

(scenario
 :describe "PTY eat: C-x C-e edits the command line in Emacs"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "echo before") (:keys "C-x C-e") (:idle 2000)
        (:keys "C-e") (:type " after") (:keys "C-x C-s C-x #") (:idle 1500)
        (:keys "<return>"))
 :then ((:terminal-shows "^before after$")))

(scenario
 :describe "PTY eat: a command spans two lines"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "for w in alpha beta; do") (:keys "<return>") (:idle 500)
        (:type "echo item-$w; done") (:keys "<return>"))
 :then ((:terminal-shows "^item-alpha\nitem-beta$")))

(scenario
 :describe "PTY eat: btop draws full screen"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000))
 :then ((:skip-unless-executables "btop")
        (:terminal-shows "cpu")
        (:terminal-shows "proc")))

(scenario
 :describe "PTY eat: q leaves btop for the prompt and the screen before it"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "echo tui-$((6*7))") (:keys "<return>") (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000) (:keys "q"))
 :then ((:skip-unless-executables "btop")
        (:terminal-shows "^tui-42\npty-project ready> btop -p 1\npty-project ready> $")))

(scenario
 :describe "PTY eat: consult-line finds and copies output scrolled out of view"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "echo scroll-$((6*7)); seq 1 300") (:keys "<return>") (:idle 1500)
        (:keys "C-c C-e C-s") (:type "scroll-42") (:keys "<return>")
        (:keys "C-a C-SPC C-e M-w"))
 :then ((:kill-ring-head "\\`scroll-42\\'")))

(scenario
 :describe "PTY eat: a narrower window resizes the terminal of the shell"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "echo resize-$((6*7))") (:keys "<return>") (:idle 500)
        (:keys "C-x 3") (:redisplay-while-typing) (:idle 1000)
        (:type "stty size") (:keys "<return>"))
 :then ((:terminal-shows "^resize-42$")
        (:terminal-size-fits-window)))

;; The control of gterm's canary: eat answers a cursor position query
;; once, with 1-based row and column.
(scenario
 :describe "PTY eat: one cursor position reply, row then column"
 :given ((:name "eat" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
        (:type "clear; printf ab'\\e[6n'; IFS= read -rs -d R r; IFS= read -rs -t 1 -d R r2; print -r -- \"cpr=${r#*\\[} dup=${r2:+yes}\"")
        (:keys "<return>"))
 :then ((:terminal-shows "cpr=1;3 dup=$")))

;;;; Known gaps of eat

(when (getenv "PTY_MATRIX_GAPS")
  ;; With its default latencies, eat renders a batch of output for up
  ;; to 700 ms, during which Emacs answers nothing: most round trips
  ;; fail the rule.  Lower `eat-maximum-latency' (0.002) and
  ;; `eat-minimum-latency' (0.001) pass it, while seq takes 3.5 s
  ;; instead of 1.5 s; that trade is left to the user.
  (scenario
   :describe "PTY eat: seq 1 200000 keeps Emacs responsive"
   :given ((:name "eat" :project "pty-project" :entry "start"))
   :when ((:keys "M-x") (:type "eat-project") (:keys "<return>") (:idle 2000)
          (:type "pty-throughput") (:keys "<return>"))
   :then ((:terminal-shows "^throughput-done in [12]?[0-9] s$" 30)
          (:round-trips-below 250))))

(provide 'l3-pty-eat)

;;; l3-pty-eat.el ends here
