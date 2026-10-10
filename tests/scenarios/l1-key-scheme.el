;;; l1-key-scheme.el --- One meaning per key (plan 093) -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 093, decision D4: a key means the same thing in every buffer
;; where it exists, and the vanilla keys the configuration had taken
;; work again.
;;
;; Paredit's keys live in paredit buffers only, so word motion is back
;; elsewhere; in Lisp buffers C-<right> still slurps, which
;; l1-paredit-elisp.el and l1-clojure-paredit.el check.  C-- is the
;; negative argument again, and expreg contracts on C-M-=.  C-c k is
;; the test runner and C-c d the debugger of a language, so pytest
;; leaves C-c t to mistty, which l3-pty-mistty.el starts with it, and
;; cargo-mode leaves C-c a to the agenda.
;;
;; Pressing C-c k t starts pytest through a shell, and C-c d d probes
;; the project interpreter for debugpy before dape prompts: neither
;; needs a program beyond the shell, and the outcomes do not depend on
;; pytest or debugpy being installed.  Opening the docker menu would
;; query the container engine, so its scenario asks Emacs what C-c D
;; runs, with C-h k, as a user would.

;;; Code:

(require 'scenario)
(require 'warnings)

;; The first LaTeX, Nix and Rust buffers of a session compile AUCTeX,
;; nix-mode and rust-mode to native code in the background.  Buttercup
;; prints the compiler's warnings with `send-string-to-terminal', which
;; fails on a graphical terminal and ends the session, as
;; tests/pty/pty-session.el explains; they say nothing about keys.
(add-to-list 'warning-suppress-log-types '(native-compiler))

(scenario
 :describe "CF-12: C-<right> moves forward a word outside Lisp, changing nothing"
 :given ((:name "text" :file "notes.txt" :text "one-!- two three\n")
         (:name "python" :file "words.py" :text "one-!- two three\n"))
 :when ((:keys "C-<right>"))
 :then ((:buffer-text "one two-!- three\n")
        (:buffer-text-unchanged)))

(scenario
 :describe "CF-12: C-<left> moves back a word outside Lisp, changing nothing"
 :given ((:name "text" :file "notes.txt" :text "one two-!- three\n")
         (:name "python" :file "words.py" :text "one two-!- three\n"))
 :when ((:keys "C-<left>"))
 :then ((:buffer-text "one -!-two three\n")
        (:buffer-text-unchanged)))

(scenario
 :describe "CF-12: C-- is a negative argument"
 :given ((:name "text" :file "notes.txt" :text "one two-!- three\n"))
 ;; M-d with the argument -1 kills the word before point.
 :when ((:keys "C-- M-d"))
 :then ((:buffer-text "one -!- three\n")))

(scenario
 :describe "CF-12: C-M-= contracts the region C-= expanded"
 :given ((:name "elisp" :file "scratch.el" :text "(defun f (x) (* va-!-lue x))"))
 :when ((:keys "C-= C-= C-M-="))
 :then ((:region-text "value")))

(scenario
 :describe "DD-02: C-c k t runs pytest on the test at point"
 :given ((:name "python"
          :file "test_app.py"
          :text "def test_one():\n    assert 1 == 1-!-\n\n\ndef test_two():\n    assert 2 == 2\n"))
 ;; The user moves to the window pytest opened, to read its run.
 :when ((:keys "C-c k t C-x o"))
 :then ((:buffer-name "\\`\\*pytest\\*")
        (:buffer-matches "test_app\\.py::test_one\\b")))

;; The other sub-keys of C-c k and C-c d are asked with C-h k: they
;; differ from their neighbours only in the scope of the run, or need
;; a debug session.
(scenario
 :describe "DD-02: C-c k f runs pytest on the file in Python"
 :given ((:name "python" :file "test_app.py" :text "def test_one():\n    pass-!-\n"))
 :when ((:keys "C-h k C-c k f C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c k f runs the command python-pytest-file-dwim (found in")))

(scenario
 :describe "DD-02: C-c k a runs pytest on the project in Python"
 :given ((:name "python" :file "test_app.py" :text "def test_one():\n    pass-!-\n"))
 :when ((:keys "C-h k C-c k a C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c k a runs the command python-pytest (found in")))

;; A Python map binding C-c t would print its own name after "found in".
(scenario
 :describe "DD-02: C-c t runs mistty in a Python buffer"
 :given ((:name "python" :file "app.py" :text "print(1)-!-\n"))
 :when ((:keys "C-h k C-c t C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c t runs the command mistty (found in global-map)")))

;; Running kaocha needs a REPL, so the Clojure scenarios ask Emacs what
;; the keys run, with C-h k, in the session's first Clojure buffer.
(scenario
 :describe "DD-02: C-c k t runs kaocha on the test at point in Clojure"
 :given ((:name "clojure" :file "test/app_test.clj" :text "(ns app-test)-!-\n"))
 :when ((:keys "C-h k C-c k t C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c k t runs the command kaocha-runner-run-test-at-point (found in")))

(scenario
 :describe "DD-02: C-c k f runs kaocha on the namespace in Clojure"
 :given ((:name "clojure" :file "test/app_test.clj" :text "(ns app-test)-!-\n"))
 :when ((:keys "C-h k C-c k f C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c k f runs the command kaocha-runner-run-tests (found in")))

;; The order matters: this scenario comes before the first one that
;; loads dape, as the keys must exist before dape loads (084 CF-15).
(scenario
 :describe "CF-15: C-c d d starts the debugger in java-mode before dape has loaded"
 :given ((:name "text" :file "Main.txt" :text "class Main {}-!-\n"))
 ;; java-mode runs where the java grammar is missing; here it is
 ;; installed, so the user turns java-mode on by hand.  A .java file
 ;; would start java-ts-mode and its language server.
 :when ((:keys "M-x") (:type "java-mode") (:keys "<return> C-c d d")
        (:sample "adapter") (:keys "C-g"))
 :then ((:mode java-mode)
        (:sampled "adapter" :minibuffer "\\`Run adapter: ")))

(scenario
 :describe "DD-02: C-c d d starts the debugger in Python"
 :given ((:name "python" :file "app.py" :text "print(1)-!-\n"))
 :when ((:keys "C-c d d") (:sample "adapter") (:keys "C-g"))
 :then ((:sampled "adapter" :minibuffer "\\`Run adapter: ")))

(scenario
 :describe "DD-02: C-c d b toggles a breakpoint in Python"
 :given ((:name "python" :file "app.py" :text "print(1)-!-\n"))
 :when ((:keys "C-h k C-c d b C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c d b runs the command dape-breakpoint-toggle (found in")))

(scenario
 :describe "DD-02: C-c d n steps to the next line in Python"
 :given ((:name "python" :file "app.py" :text "print(1)-!-\n"))
 :when ((:keys "C-h k C-c d n C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c d n runs the command dape-next (found in")))

(scenario
 :describe "DD-02: C-c a opens the agenda dispatcher in a Rust buffer"
 :given ((:name "rust" :file "src/main.rs" :text "fn main() {}-!-\n"))
 ;; q leaves the dispatcher; only the agenda's says Abort.
 :when ((:keys "C-c a q"))
 :then ((:user-error "\\`Abort\\'")))

(scenario
 :describe "CF-09: C-c n u runs nix-shell-unpack"
 :given ((:name "nix" :file "pkgs/hello.nix" :text "{ }-!-\n"))
 ;; nix-build and the nix-shell commands ask the same "Nix file: " as
 ;; nix-shell-unpack: the user asks what the key runs.
 :when ((:keys "C-h k C-c n u C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c n u runs the command nix-shell-unpack (found in")))

(scenario
 :describe "DD-02: cargo's commands are under C-c C in a Rust buffer"
 :given ((:name "rust" :file "src/main.rs" :text "fn main() {}-!-\n"))
 ;; Running cargo-mode-test would run cargo: the user asks what the key runs.
 :when ((:keys "C-h k C-c C t C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c C t runs the command cargo-mode-test (found in")))

(scenario
 :describe "DD-02: C-c keys reach AUCTeX in a LaTeX buffer"
 :given ((:name "latex"
          :file "paper.tex"
          :text "\\documentclass{article}\n\\begin{document}\n-!-\n\\end{document}\n"))
 :when ((:keys "C-c C-s") (:sample "level") (:keys "C-g"))
 :then ((:sampled "level" :minibuffer "Level")))

(scenario
 :describe "DD-02: <f7> is cdlatex's prefix of math accents and fonts"
 :given ((:name "latex"
          :file "paper.tex"
          :text "\\documentclass{article}\n\\begin{document}\n$x-!-$\n\\end{document}\n"))
 :when ((:keys "<f7> b"))
 :then ((:buffer-matches "\\$\\\\mathbf{x}\\$")))

(scenario
 :describe "DD-02: C-c D runs docker"
 :given ((:name "text" :file "notes.txt" :text "notes-!-\n"))
 ;; The user reads the answer in the help window.
 :when ((:keys "C-h k C-c D C-x o"))
 :then ((:buffer-name "\\`\\*Help\\*\\'")
        (:buffer-matches "\\`C-c D runs the command docker (found in")))

(scenario
 :describe "DD-02: C-c d is unbound outside the debugger buffers"
 :given ((:name "text" :file "notes.txt" :text "-!-\n"))
 ;; With C-u, C-h c inserts its answer at point.
 :when ((:keys "C-u C-h c C-c d"))
 :then ((:buffer-text "C-c d is undefined-!-\n")))

(provide 'l1-key-scheme)

;;; l1-key-scheme.el ends here
