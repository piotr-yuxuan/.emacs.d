;;; l1-apheleia-format-key.el --- C-c r formats the buffer through apheleia -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 071 / apheleia slice, TS-18.  The live config enables
;; `apheleia-mode' in `prog-mode' and binds `apheleia-format-buffer' to
;; C-c r in `prog-mode-map'.  For Emacs Lisp apheleia formats in
;; process, with its `lisp-indent' formatter, so the key is testable
;; without a formatter binary; apheleia still runs diff to apply the
;; result, so the scenario waits briefly after the key.
;;
;; An earlier version only checked that `apheleia-mode' was on, on the
;; false premise that C-c prefix keys are lost in tests.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-APHELEIA-FORMAT-KEY: C-c r reindents a badly indented Emacs Lisp file"
 :given ((:name "el"
          :file "sample.el"
          :text "(defun f (x)\n(* x-!- x))\n"))
 :when ((:keys "C-c r") (:idle 1500))
 :then ((:buffer-text "(defun f (x)\n  (* x x))\n")
        (:minor-modes-on apheleia-mode)))

(provide 'l1-apheleia-format-key)

;;; l1-apheleia-format-key.el ends here
