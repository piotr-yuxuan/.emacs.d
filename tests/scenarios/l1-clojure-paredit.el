;;; l1-clojure-paredit.el --- Paredit slurp/barf/splice across the Clojure family -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 062 / paredit slice.  Presses `C-<right>' (slurp), `C-<left>'
;; (barf) and `M-s' (splice) on file-backed buffers and asserts the
;; user-visible buffer text and point afterwards.
;;
;; The slurp regression from plan 052 (LSP-hijacked `indent-region')
;; is exercised at L3 by `l3-clojure-paredit-lsp.el'; this file covers
;; the L1 invariants: slurp moves the closing paren without corrupting
;; the buffer text, and point stays where the user left it.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-CLJ-PAREDIT-SLURP: C-<right> slurps the next sexp"
 :given ((:name "clj"  :file "src/example.clj"  :text "(+ 1-!-) 2")
         (:name "cljs" :file "src/example.cljs" :text "(+ 1-!-) 2")
         (:name "cljc" :file "src/example.cljc" :text "(+ 1-!-) 2")
         (:name "bb"   :file "scripts/example.bb" :text "(+ 1-!-) 2"))
 :when ((:keys "C-<right>"))
 ;; Paredit moves the closing paren, not point.
 :then ((:buffer-text "(+ 1-!- 2)")
        (:balanced)))

(scenario
 :describe "BL1-CLJ-PAREDIT-SLURP-USER-CASE-2: (list| :a) :b :c -> (list :a :b) :c"
 :given ((:name "list-form-clj"
          :file "src/example.clj"
          :text "(list-!- :a) :b :c"))
 :when ((:keys "C-<right>"))
 :then ((:buffer-text "(list-!- :a :b) :c")
        (:balanced)))

(scenario
 :describe "BL1-CLJ-PAREDIT-BARF: C-<left> barfs the last sexp out of the form"
 :given ((:name "barf-clj"
          :file "src/example.clj"
          :text "(+ 1-!- 2)"))
 :when ((:keys "C-<left>"))
 :then ((:buffer-text "(+ 1) 2")
        (:balanced)))

(scenario
 :describe "BL1-CLJ-PAREDIT-SPLICE: M-s splices the surrounding form away"
 :given ((:name "splice-clj"
          :file "src/example.clj"
          :text "(when true (do-!- something))"))
 :when ((:keys "M-s"))
 :then ((:buffer-text "(when true do something)")
        (:balanced)))

(provide 'l1-clojure-paredit)

;;; l1-clojure-paredit.el ends here
