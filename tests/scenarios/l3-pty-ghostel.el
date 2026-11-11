;;; l3-pty-ghostel.el --- The terminal parity matrix: ghostel -*- lexical-binding: t -*-
;; scenario-tier: l3
;; scenario-display: graphic

;;; Commentary:
;; The terminal parity matrix of plan 091, kept by plan 110 for ghostel,
;; the one terminal left.  Each scenario is one capability, with the
;; keys and outcomes eat's row had: C-c t opens ghostel in the
;; directory of the current buffer, C-c T at the project's root, and
;; ghostel's emacs mode (C-c C-e) leaves its scrollback to Emacs's
;; search.  fzf drops the keys typed before it has drawn its counter,
;; so its scenarios wait for that counter instead of a fixed time.
;;
;; The shell is zsh with the fixture startup files of
;; tests/fixtures/pty-zdotdir/ (see tests/pty/pty-session.el), in a
;; copy of tests/fixtures/pty-project/.  Ghostel needs its native
;; module, which the tests never download: without it, every scenario
;; fails.  nix/setup.sh modules fetches it, as the first M-x ghostel
;; does.
;;
;; The throughput scenario times the terminal against the wall clock,
;; so a busy machine fails it.  It is defined only when the variable
;; PTY_MATRIX_TIMING is set, for a run on an otherwise idle machine:
;; PTY_MATRIX_TIMING=1 tests/run full l3-pty.  Known gaps, if any, are
;; defined only when PTY_MATRIX_GAPS is set, so that `tests/run full'
;; stays green: PTY_MATRIX_GAPS=1 tests/run full l3-pty.

;;; Code:

(require 'scenario)
(require 'pty-session (expand-file-name "../pty/pty-session.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))

(scenario
 :describe "PTY ghostel: opens at the project root, prompt ready"
 :given ((:name "ghostel" :project "pty-project" :entry "sub/pty-unique-file.txt"))
 :when ((:keys "C-c T"))
 :then ((:terminal-shows "^pty-project ready> ")
        (:terminal-directory "../")))

(scenario
 :describe "PTY ghostel: tracks the directory after cd"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "cd sub") (:keys "<return>"))
 :then ((:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY ghostel: C-r picks a command from the history with fzf"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "C-r") (:idle-until (:terminal-now-shows "[0-9]+/[0-9]+") 10000)
        (:type "history-pick") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^history-pick$")))

(scenario
 :describe "PTY ghostel: C-t inserts a file path with fzf"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo ") (:keys "C-t") (:idle-until (:terminal-now-shows "[0-9]+/[0-9]+") 10000)
        (:type "pty-unique") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^sub/pty-unique-file\\.txt$")))

(scenario
 :describe "PTY ghostel: M-c changes to a directory with fzf"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "M-c") (:idle-until (:terminal-now-shows "[0-9]+/[0-9]+") 10000)
        (:type "sub") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY ghostel: M-<left> and M-<right> move by words"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo alpha beta") (:idle 300)
        (:keys "M-<left>") (:idle 300) (:keys "M-<left>") (:idle 300)
        (:keys "M-<right>") (:idle 300) (:type "Y") (:idle 300) (:keys "<return>"))
 :then ((:terminal-shows "^alpha Ybeta$")))

(scenario
 :describe "PTY ghostel: M-<backspace> and C-<backspace> delete words"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo alpha beta gamma") (:idle 300)
        (:keys "M-<backspace>") (:idle 300) (:keys "C-<backspace>") (:idle 300)
        (:type "delta") (:idle 300) (:keys "<return>"))
 :then ((:terminal-shows "^alpha delta$")))

(scenario
 :describe "PTY ghostel: C-x C-e edits the command line in Emacs"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo before") (:keys "C-x C-e") (:idle 2000)
        (:keys "C-e") (:type " after") (:keys "C-x C-s C-x #") (:idle 1500)
        (:keys "<return>"))
 :then ((:terminal-shows "^before after$")))

(scenario
 :describe "PTY ghostel: a command spans two lines"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "for w in alpha beta; do") (:keys "<return>") (:idle 500)
        (:type "echo item-$w; done") (:keys "<return>"))
 :then ((:terminal-shows "^item-alpha\nitem-beta$")))

(scenario
 :describe "PTY ghostel: btop draws full screen"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000))
 :then ((:skip-unless-executables "btop")
        (:terminal-shows "cpu")
        (:terminal-shows "proc")))

(scenario
 :describe "PTY ghostel: q leaves btop for the prompt and the screen before it"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "echo tui-$((6*7))") (:keys "<return>") (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000) (:keys "q"))
 :then ((:skip-unless-executables "btop")
        (:terminal-shows "^tui-42\npty-project ready> btop -p 1\npty-project ready> $")))

(scenario
 :describe "PTY ghostel: M-x my/btop draws btop full screen"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:keys "M-x") (:type "my/btop") (:keys "<return>") (:idle 3000))
 :then ((:skip-unless-executables "btop")
        (:terminal-shows "cpu")
        (:terminal-shows "proc")))

(scenario
 :describe "PTY ghostel: a second M-x my/btop returns to the btop running"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:keys "M-x") (:type "my/btop") (:keys "<return>") (:idle 3000)
        (:keys "C-x b <return>") (:idle 500)
        (:keys "M-x") (:type "my/btop") (:keys "<return>") (:idle 1000))
 :then ((:skip-unless-executables "btop")
        (:buffer-name "\\`\\*ghostel\\* btop\\'")
        (:terminal-shows "cpu")))

(scenario
 :describe "PTY ghostel: consult-line finds and copies output scrolled out of view"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo scroll-$((6*7)); seq 1 300") (:keys "<return>") (:idle 1500)
        (:keys "C-c C-e C-s") (:type "scroll-42") (:keys "<return>")
        (:keys "C-a C-SPC C-e M-w"))
 :then ((:kill-ring-head "\\`scroll-42\\'")))

(scenario
 :describe "PTY ghostel: a narrower window resizes the terminal of the shell"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo resize-$((6*7))") (:keys "<return>") (:idle 500)
        (:keys "C-x 3") (:redisplay-while-typing) (:idle 1000)
        (:type "stty size") (:keys "<return>"))
 :then ((:terminal-shows "^resize-42$")
        (:terminal-size-fits-window)))

;; fzf asks where the cursor is before it draws, and waits for one
;; reply, with 1-based row and column.
(scenario
 :describe "PTY ghostel: one cursor position reply, row then column"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "clear; printf ab'\\e[6n'; IFS= read -rs -d R r; IFS= read -rs -t 1 -d R r2; print -r -- \"cpr=${r#*\\[} dup=${r2:+yes}\"")
        (:keys "<return>"))
 :then ((:terminal-shows "cpr=1;3 dup=$")))

;; Semi-char mode sends the function keys to the shell, but for the
;; ones the configuration keeps for itself.
(scenario
 :describe "PTY ghostel: <f8> still opens treemacs"
 :given ((:name "ghostel" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "C-h k <f8> C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`<f8> runs the command treemacs (found in global-map)")))

;;;; Timing of ghostel

(when (getenv "PTY_MATRIX_TIMING")
  ;; Ghostel prints the 200000 lines of the other terminals' run in
  ;; under a second, too fast for the five round trips the rule needs.
  (scenario
   :describe "PTY ghostel: seq 1 2000000 keeps Emacs responsive"
   :given ((:name "ghostel" :project "pty-project" :entry "start"))
   :when ((:keys "C-c t") (:idle 2000)
          (:type "pty-throughput 2000000") (:keys "<return>"))
   :then ((:terminal-shows "^throughput-done in [12]?[0-9] s$" 30)
          (:round-trips-below 250))))

(provide 'l3-pty-ghostel)

;;; l3-pty-ghostel.el ends here
