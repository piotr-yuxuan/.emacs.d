;;; l1-expreg.el --- C-= grows the region across syntax nodes -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 073 / expreg slice.  The live config binds `expreg-expand' to
;; C-=.  expreg uses tree-sitter, or the syntax table where no parser
;; exists, to grow the active region across structurally meaningful
;; units.
;;
;; The scenario sets point inside a leaf token, presses C-= once, and
;; asserts the region now holds the symbol around point, which a mere
;; `set-mark-command' (an empty region) would not.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-EXPREG-EXPAND: C-= grows the region around the symbol at point"
 :given ((:name "balanced-elisp"
          :file "scratch.el"
          :text "(defun f (x) (* va-!-lue x))"))
 :when ((:keys "C-="))
 :then ((:region-text "value")))

(provide 'l1-expreg)

;;; l1-expreg.el ends here
