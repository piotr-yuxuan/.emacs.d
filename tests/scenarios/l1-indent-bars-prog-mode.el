;;; l1-indent-bars-prog-mode.el --- Plan 071: indent-bars activates in prog-mode -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 071 / indent-bars slice.  The carry-forward (plan 061) records
;; one user-visible invariant for indent-bars: the mode is enabled in
;; every buffer derived from `prog-mode'.  The live config attaches
;; `indent-bars-mode' to `prog-mode-hook' and `LaTeX-mode-hook'.
;;
;; Opening a Python, shell or Emacs Lisp file must produce a buffer
;; where `indent-bars-mode' is on.  The bars themselves are drawn by
;; redisplay in a graphical frame.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-INDENT-BARS-PROG-MODE: opening a prog-mode file activates indent-bars-mode"
 :given ((:name "py"
          :file "sample.py"
          :text "def f(x):\n    return x * x\n")
         (:name "sh"
          :file "sample.sh"
          :text "#!/usr/bin/env bash\nfor i in 1 2 3; do\n  echo $i\ndone\n")
         (:name "el"
          :file "sample.el"
          :text "(defun f (x)\n  (* x x))\n"))
 :when ((:open))
 :then ((:mode prog-mode)
        (:minor-modes-on indent-bars-mode)))

(provide 'l1-indent-bars-prog-mode)

;;; l1-indent-bars-prog-mode.el ends here
