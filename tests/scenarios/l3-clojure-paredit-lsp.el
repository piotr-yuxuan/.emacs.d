;;; l3-clojure-paredit-lsp.el --- Paredit-under-LSP regression scenarios -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 062 / paredit-under-LSP slice.  Plan 052 fixed a regression
;; where `lsp-mode' hijacked `indent-region-function' under
;; `clojure-mode', so that paredit's slurp / barf rounded the buffer
;; through clojure-lsp's range-formatting and corrupted it.  These
;; scenarios are the behavioural regression check: with a real
;; `clojure-lsp' attached, `C-<right>' must produce the same result
;; as it does without LSP, and the buffer's parenthesis structure
;; must remain balanced.
;;
;; Each row adds its project to the private lsp session, as the user
;; does when lsp-mode first asks for the project root, and waits, as
;; an idle user, until `lsp-deferred' has attached clojure-lsp.  A
;; missing clojure-lsp skips the spec.
;;
;; The fixture is `tests/fixtures/clojure-paredit/'.  `:point-on "1)
;; 2"' puts point on the `1' just before the inner sexp's closing paren
;; of `(defn add ... (+ 1) 2)'; the longer needle dodges the
;; docstring's literal `(+ 1)'.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL3-CLJ-PAREDIT-SLURP-LSP: under live clojure-lsp, slurp does not corrupt the buffer (.clj)"
 :given ((:name "clj-with-lsp"
          :project "clojure-paredit"
          :entry "src/example.clj"
          :point-on "1) 2"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp))
 :when ((:keys "C-<right>"))
 :then ((:buffer-matches "(\\+ 1 2)")
        ;; Plan 052 regression: with lsp-enable-indentation nil the slurp
        ;; does not round-trip through clojure-lsp's rangeFormatting.
        (:balanced)
        (:mode clojure-mode)
        ;; The fixture reads `(+ 1) 2': a no-op slurp fails here even if a
        ;; future fixture already matched the regexp above.
        (:buffer-text-changed)))

(scenario
 :describe "BL3-CLJC-PAREDIT-SLURP-LSP: cljc variant under live clojure-lsp"
 :given ((:name "cljc-with-lsp"
          :project "clojure-paredit"
          :entry "src/example.cljc"
          :point-on "1) 2"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp))
 :when ((:keys "C-<right>"))
 :then ((:buffer-matches "(\\+ 1 2)")
        (:balanced)
        (:mode clojure-mode)
        (:buffer-text-changed)))

(provide 'l3-clojure-paredit-lsp)

;;; l3-clojure-paredit-lsp.el ends here
