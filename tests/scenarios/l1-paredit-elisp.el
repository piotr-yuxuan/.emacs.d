;;; l1-paredit-elisp.el --- Paredit slurp/barf/splice in emacs-lisp-mode -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 073 / paredit slice (emacs-lisp).  Plan 062 ships paredit
;; slurp / barf / splice scenarios in clojure-mode; this file presses
;; the same generic paredit bindings in an emacs-lisp-mode buffer to
;; confirm the hook chain wires paredit outside the Clojure cluster:
;;
;;   C-<right>  paredit-forward-slurp-sexp
;;   C-<left>   paredit-forward-barf-sexp
;;   C-s-s      paredit-splice-sexp

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-PAREDIT-ELISP-SLURP: C-<right> slurps the next sexp in elisp"
 :given ((:name "elisp-slurp"
          :file "scratch.el"
          :text "(+ 1-!-) 2"))
 :when ((:keys "C-<right>"))
 :then ((:buffer-text "(+ 1-!- 2)")
        (:balanced)))

(scenario
 :describe "BL1-PAREDIT-ELISP-BARF: C-<left> barfs the last sexp out of the form"
 :given ((:name "elisp-barf"
          :file "scratch.el"
          :text "(+ 1-!- 2)"))
 :when ((:keys "C-<left>"))
 :then ((:buffer-text "(+ 1) 2")
        (:balanced)))

(scenario
 :describe "BL1-PAREDIT-ELISP-SPLICE: C-s-s splices the surrounding form away"
 :given ((:name "elisp-splice"
          :file "scratch.el"
          :text "(when t (do-!- something))"))
 :when ((:keys "C-s-s"))
 :then ((:buffer-text "(when t do something)")
        (:balanced)))

(provide 'l1-paredit-elisp)

;;; l1-paredit-elisp.el ends here
