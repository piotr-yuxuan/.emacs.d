;;; l3-pty-gterm.el --- The terminal parity matrix: gterm -*- lexical-binding: t -*-
;; scenario-tier: l3
;; scenario-display: graphic

;;; Commentary:
;; The gterm row of the terminal parity matrix of plan 091.  Each
;; scenario is one capability, with the same keys and outcomes in
;; l3-pty-eat.el, l3-pty-mistty.el and l3-pty-gterm.el, except
;; how the terminal opens, and how its output is searched: from its
;; copy mode (C-c C-k).  Each scenario skips while gterm's module is
;; not built.  The canary at the end fails once upstream fixes the
;; Ctrl-key bindings that the configuration rebinds.
;;
;; The shell is zsh with the fixture startup files of
;; tests/fixtures/pty-zdotdir/ (see tests/pty/pty-session.el), in a
;; copy of tests/fixtures/pty-project/.
;;
;; The scenarios of the capabilities this terminal lacks, its known
;; gaps, are defined only when the environment variable
;; PTY_MATRIX_GAPS is set, so that `tests/run full' stays green:
;; PTY_MATRIX_GAPS=1 tests/run full l3-pty.  A gap that passes there
;; may be closed; check it again, then move its scenario up.
;;
;; The throughput scenario times the terminal against the wall clock,
;; so a busy machine fails it.  It is defined only when the variable
;; PTY_MATRIX_TIMING is set, for a run on an otherwise idle machine:
;; PTY_MATRIX_TIMING=1 tests/run full l3-pty.

;;; Code:

(require 'scenario)
(require 'pty-session (expand-file-name "../pty/pty-session.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))

(scenario
 :describe "PTY gterm: tracks the directory after cd"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "cd sub") (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY gterm: C-r picks a command from the history with fzf"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:keys "C-r") (:idle 1000) (:type "history-pick") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^history-pick$")))

(scenario
 :describe "PTY gterm: C-t inserts a file path with fzf"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "echo ") (:keys "C-t") (:idle 1000) (:type "pty-unique") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^sub/pty-unique-file\\.txt$")))

(scenario
 :describe "PTY gterm: M-c changes to a directory with fzf"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:keys "M-c") (:idle 1000) (:type "sub") (:idle 500) (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY gterm: M-<left> and M-<right> move by words"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "echo alpha beta") (:idle 300)
        (:keys "M-<left>") (:idle 300) (:keys "M-<left>") (:idle 300)
        (:keys "M-<right>") (:idle 300) (:type "Y") (:idle 300) (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^alpha Ybeta$")))

(scenario
 :describe "PTY gterm: M-<backspace> and C-<backspace> delete words"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "echo alpha beta gamma") (:idle 300)
        (:keys "M-<backspace>") (:idle 300) (:keys "C-<backspace>") (:idle 300)
        (:type "delta") (:idle 300) (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^alpha delta$")))

(scenario
 :describe "PTY gterm: C-x C-e edits the command line in Emacs"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "echo before") (:keys "C-x C-e") (:idle 2000)
        (:keys "C-e") (:type " after") (:keys "C-x C-s C-x #") (:idle 1500)
        (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^before after$")))

(scenario
 :describe "PTY gterm: a command spans two lines"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "for w in alpha beta; do") (:keys "<return>") (:idle 500)
        (:type "echo item-$w; done") (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^item-alpha\nitem-beta$")))

(scenario
 :describe "PTY gterm: btop draws full screen"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000))
 :then ((:skip-unless-gterm-module)
        (:skip-unless-executables "btop")
        (:terminal-shows "cpu")
        (:terminal-shows "proc")))

(scenario
 :describe "PTY gterm: q leaves btop for the prompt and the screen before it"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "echo tui-$((6*7))") (:keys "<return>") (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000) (:keys "q"))
 :then ((:skip-unless-gterm-module)
        (:skip-unless-executables "btop")
        (:terminal-shows "^tui-42\npty-project ready> btop -p 1\npty-project ready> $")))

(scenario
 :describe "PTY gterm: a narrower window resizes the terminal of the shell"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "echo resize-$((6*7))") (:keys "<return>") (:idle 500)
        (:keys "C-x 3") (:redisplay-while-typing) (:idle 1000)
        (:type "stty size") (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^resize-42$")
        (:terminal-size-fits-window)))

(scenario
 :describe "PTY gterm: canary, upstream still binds Ctrl and a letter as two events"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:key-sequence-bound gterm-mode-map [control 97] gterm-send-ctrl-key)))

;; Canary: one reply, with 1-based row and column, to a cursor
;; position query.  Fails on a missing, swapped or 0-based reply, and
;; on a second one once upstream answers the query itself.
(scenario
 :describe "PTY gterm: canary, one cursor position reply, row then column"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "clear; printf ab'\\e[6n'; IFS= read -rs -d R r; IFS= read -rs -t 1 -d R r2; print -r -- \"cpr=${r#*\\[} dup=${r2:+yes}\"")
        (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "cpr=1;3 dup=$")))

;; A program can print any OSC 7 report.  A remote path must neither
;; become the directory nor open a TRAMP connection.  TRAMP kills its
;; buffer after a failed connection, so its message is what is left.
(scenario
 :describe "PTY gterm: ignores a directory report with a remote path"
 :given ((:name "gterm" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
        (:type "printf '\\e]7;file://h/ssh:x.invalid:/\\e\\\\'; echo osc-$((6*7))")
        (:keys "<return>"))
 :then ((:skip-unless-gterm-module)
        (:terminal-shows "^osc-42$")
        (:terminal-directory "./")
        (:messages-exclude "Tramp")))

;;;; Timing of gterm

(when (getenv "PTY_MATRIX_TIMING")
  ;; seq takes 18 s on an idle machine, the 30 s deadline less than
  ;; twice that.
  (scenario
   :describe "PTY gterm: seq 1 200000 keeps Emacs responsive"
   :given ((:name "gterm" :project "pty-project" :entry "start"))
   :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
          (:type "pty-throughput") (:keys "<return>"))
   :then ((:skip-unless-gterm-module)
          (:terminal-shows "^throughput-done in [12]?[0-9] s$" 30)
          (:round-trips-below 250))))

;;;; Known gaps of gterm

(when (getenv "PTY_MATRIX_GAPS")
  ;; `gterm' starts in the directory of the current buffer, not at its
  ;; project's root as eat-project does.
  (scenario
   :describe "PTY gterm: opens at the project root, prompt ready"
   :given ((:name "gterm" :project "pty-project" :entry "sub/pty-unique-file.txt"))
   :when ((:keys "M-x") (:type "gterm") (:keys "<return>"))
   :then ((:skip-unless-gterm-module)
          (:terminal-shows "^pty-project ready> ")
          (:terminal-directory "../")))

  ;; gterm's buffer holds the visible screen only: consult-line cannot
  ;; find output scrolled out of view, so the keys end inside its prompt.
  ;; Paging with S-<prior> is gterm's only way back.
  (scenario
   :describe "PTY gterm: consult-line finds and copies output scrolled out of view"
   :given ((:name "gterm" :project "pty-project" :entry "start"))
   :when ((:keys "M-x") (:type "gterm") (:keys "<return>") (:idle 2000)
          (:type "echo scroll-$((6*7)); seq 1 300") (:keys "<return>") (:idle 1500)
          (:keys "C-c C-k C-s") (:type "scroll-42") (:keys "<return>")
          (:keys "C-a C-SPC C-e M-w"))
   :then ((:skip-unless-gterm-module)
          (:kill-ring-head "\\`scroll-42\\'"))))

(provide 'l3-pty-gterm)

;;; l3-pty-gterm.el ends here
