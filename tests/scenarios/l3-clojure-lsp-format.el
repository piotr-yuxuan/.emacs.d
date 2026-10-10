;;; l3-clojure-lsp-format.el --- lsp-format-buffer scenarios -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 062b lsp-format-buffer slice.  The `clojure-paredit' fixture
;; ships a `weird-spacing' defn whose body carries non-canonical
;; spacing and indentation.  The user's daily fix for that shape is
;; `lsp-format-buffer', bound to `C-c l = =' under `lsp-command-map'.
;; Two scenarios assert that the format round-trip rewrites the form to
;; its canonical shape and leaves the delimiters balanced.
;;
;; The fixture ships `.cljfmt.edn' with
;; `{:remove-multiple-non-indenting-spaces? true}', so cljfmt collapses
;; every inner whitespace gap (its default profile keeps them).  The
;; canonical post-format `weird-spacing' is:
;;
;;   (defn weird-spacing
;;     "...docstring..."
;;     [x]
;;     (* x x))
;;
;; Each row adds its project to the private lsp session, as the user
;; does when lsp-mode first asks for the project root, then waits for
;; clojure-lsp to attach and finish indexing.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL3-CLJ-LSP-FORMAT-BUFFER-INDENTS: C-c l = = rewrites the body indent"
 :given ((:name "format-indents"
          :project "clojure-paredit"
          :entry "src/example.clj"
          :point-on "weird-spacing"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "C-c l = ="))
 :then ((:buffer-matches "^  \\[x\\]$")
        (:buffer-matches "^  (\\* x x))$")
        ;; A silent no-op format fails here even if a future fixture
        ;; already matched the canonical regexps.
        (:buffer-text-changed)))

(scenario
 :describe "BL3-CLJ-LSP-FORMAT-BUFFER-BALANCED: C-c l = = leaves the buffer balanced"
 :given ((:name "format-balanced"
          :project "clojure-paredit"
          :entry "src/example.clj"
          :point-on "weird-spacing"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "C-c l = ="))
 :then ((:balanced)
        (:mode clojure-mode)
        ;; The header `( defn  weird-spacing' becomes canonical.
        (:buffer-matches "^(defn weird-spacing$")
        (:buffer-text-changed)))

(provide 'l3-clojure-lsp-format)

;;; l3-clojure-lsp-format.el ends here
