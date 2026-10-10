;;; l3-pty-mistty.el --- The terminal parity matrix: mistty -*- lexical-binding: t -*-
;; scenario-tier: l3
;; scenario-display: graphic

;;; Commentary:
;; The mistty row of the terminal parity matrix of plan 091.  Each
;; scenario is one capability, with the same keys and outcomes in
;; l3-pty-eat.el, l3-pty-mistty.el and l3-pty-gterm.el, except
;; how the terminal opens.
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
;; The throughput scenario, a gap, times the terminal against the
;; wall clock, so run the gaps on an otherwise idle machine.

;;; Code:

(require 'scenario)
(require 'pty-session (expand-file-name "../pty/pty-session.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))

(scenario
 :describe "PTY mistty: tracks the directory after cd"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "cd sub") (:keys "<return>"))
 :then ((:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY mistty: C-r picks a command from the history with fzf"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "C-r") (:idle 1000) (:type "history-pick") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^history-pick$")))

(scenario
 :describe "PTY mistty: C-t inserts a file path with fzf"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo ") (:keys "C-t") (:idle 1000) (:type "pty-unique") (:idle 500)
        (:keys "<return>") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^sub/pty-unique-file\\.txt$")))

(scenario
 :describe "PTY mistty: M-c changes to a directory with fzf"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "M-c") (:idle 1000) (:type "sub") (:idle 500) (:keys "<return>"))
 :then ((:terminal-shows "^sub ready> ")
        (:terminal-directory "sub/")))

(scenario
 :describe "PTY mistty: M-<left> and M-<right> move by words"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo alpha beta") (:idle 300)
        (:keys "M-<left>") (:idle 300) (:keys "M-<left>") (:idle 300)
        (:keys "M-<right>") (:idle 300) (:type "Y") (:idle 300) (:keys "<return>"))
 :then ((:terminal-shows "^alpha Ybeta$")))

(scenario
 :describe "PTY mistty: M-<backspace> and C-<backspace> delete words"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo alpha beta gamma") (:idle 300)
        (:keys "M-<backspace>") (:idle 300) (:keys "C-<backspace>") (:idle 300)
        (:type "delta") (:idle 300) (:keys "<return>"))
 :then ((:terminal-shows "^alpha delta$")))

(scenario
 :describe "PTY mistty: C-x C-e edits the command line in Emacs"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo before") (:keys "C-x C-e") (:idle 2000)
        (:keys "C-e") (:type " after") (:keys "C-x C-s C-x #") (:idle 1500)
        (:keys "<return>"))
 :then ((:terminal-shows "^before after$")))

(scenario
 :describe "PTY mistty: a command spans two lines"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "for w in alpha beta; do") (:keys "<return>") (:idle 500)
        (:type "echo item-$w; done") (:keys "<return>"))
 :then ((:terminal-shows "^item-alpha\nitem-beta$")))

(scenario
 :describe "PTY mistty: btop draws full screen"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000))
 :then ((:skip-unless-executables "btop")
        (:terminal-shows "cpu")
        (:terminal-shows "proc")))

(scenario
 :describe "PTY mistty: q leaves btop for the prompt and the screen before it"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:keys "C-x 1") (:redisplay-while-typing) (:idle 500)
        (:type "echo tui-$((6*7))") (:keys "<return>") (:idle 500)
        (:type "btop -p 1") (:keys "<return>") (:idle 3000) (:keys "q"))
 :then ((:skip-unless-executables "btop")
        (:terminal-shows "^tui-42\npty-project ready> btop -p 1\npty-project ready> $")))

(scenario
 :describe "PTY mistty: consult-line finds and copies output scrolled out of view"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo scroll-$((6*7)); seq 1 300") (:keys "<return>") (:idle 1500)
        (:keys "C-s") (:type "scroll-42") (:keys "<return>")
        (:keys "C-a C-SPC C-e M-w"))
 :then ((:kill-ring-head "\\`scroll-42\\'")))

(scenario
 :describe "PTY mistty: a narrower window resizes the terminal of the shell"
 :given ((:name "mistty" :project "pty-project" :entry "start"))
 :when ((:keys "C-c t") (:idle 2000)
        (:type "echo resize-$((6*7))") (:keys "<return>") (:idle 500)
        (:keys "C-x 3") (:redisplay-while-typing) (:idle 1000)
        (:type "stty size") (:keys "<return>"))
 :then ((:terminal-shows "^resize-42$")
        (:terminal-size-fits-window)))

;;;; Known gaps of mistty

(when (getenv "PTY_MATRIX_GAPS")
  ;; C-c t runs `mistty', which starts in the directory of the current
  ;; buffer, not at its project's root as eat-project does.
  (scenario
   :describe "PTY mistty: opens at the project root, prompt ready"
   :given ((:name "mistty" :project "pty-project" :entry "sub/pty-unique-file.txt"))
   :when ((:keys "C-c t"))
   :then ((:terminal-shows "^pty-project ready> ")
          (:terminal-directory "../"))))

;;;; Timing gap of mistty

(when (getenv "PTY_MATRIX_GAPS")
  ;; mistty's built-in backend, term.el, renders output in Lisp in its
  ;; process filter.  While seq prints, 3 to 11 round trips in 20 took
  ;; 250 ms or more, up to 1.2 s, in three runs of four at a load
  ;; average of 8 to 17; the fourth passed.  The gap is narrow.
  ;; mistty's alacritty backend, a Rust module, may close it.
  (scenario
   :describe "PTY mistty: seq 1 200000 keeps Emacs responsive"
   :given ((:name "mistty" :project "pty-project" :entry "start"))
   :when ((:keys "C-c t") (:idle 2000)
          (:type "pty-throughput") (:keys "<return>"))
   :then ((:terminal-shows "^throughput-done in [12]?[0-9] s$" 30)
          (:round-trips-below 250))))

(provide 'l3-pty-mistty)

;;; l3-pty-mistty.el ends here
