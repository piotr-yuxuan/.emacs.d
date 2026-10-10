;;; l3-clojure-lsp-navigation.el --- Cross-namespace M-./C-c l g r scenarios -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 062b cross-namespace navigation slice: `M-.' and
;; `lsp-find-references' under a live `clojure-lsp'.
;;
;; - `M-.' is the daily binding for `xref-find-definitions';
;;   `lsp-mode' registers an xref backend, so it lands at the defn.
;;   The outcome checks the line point lands on (TS-11).
;; - `M-?' is shadowed by `paredit-convolute-sexp' in every
;;   `clojure-mode' buffer, so references go through `C-c l g r' under
;;   `lsp-command-map'.  The *xref* buffer lists the file of each hit.
;;
;; Each row adds its project to the private lsp session and waits,
;; as an idle user, for clojure-lsp to attach and finish indexing, so
;; that `M-.' cannot race the server's initial project scan.
;;
;; The fixture `tests/fixtures/clojure-nav/' refers to
;; `acme.another/quux' from `.clj' and `.cljc' entry points.
;; `:point-on "another/quux"' lands point on the symbol itself; a bare
;; "quux" would first match the `use-quux' defn name.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL3-CLJ-LSP-FIND-DEFINITION-CLJ: M-. on another/quux from .clj lands on the defn"
 :given ((:name "clj-find-definition"
          :project "clojure-nav"
          :entry "src/acme/core.clj"
          :point-on "another/quux"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "M-."))
 :then ((:file-suffix "src/acme/another.clj")
        (:line-at-point "\\`(defn quux\\'")))

(scenario
 :describe "BL3-CLJ-LSP-FIND-DEFINITION-CLJC: M-. on another/quux from .cljc lands on the same defn"
 :given ((:name "cljc-find-definition"
          :project "clojure-nav"
          :entry "src/acme/core.cljc"
          :point-on "another/quux"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "M-."))
 :then ((:file-suffix "src/acme/another.clj")
        (:line-at-point "\\`(defn quux\\'")))

(scenario
 :describe "BL3-CLJ-LSP-FIND-REFERENCES-DEFN: C-c l g r on the quux defn lists the cross-namespace call site"
 :given ((:name "find-references-on-defn"
          :project "clojure-nav"
          :entry "src/acme/another.clj"
          ;; In another.clj the first `quux' is the defn name.
          :point-on "quux"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "C-c l g r"))
 :then ((:buffer-name "\\`\\*xref\\*\\'")
        (:buffer-matches "src/acme/core\\.clj")))

(scenario
 :describe "BL3-CLJ-LSP-FIND-REFERENCES-VAR: C-c l g r on the call-site var lists itself"
 :given ((:name "find-references-on-call-site"
          :project "clojure-nav"
          :entry "src/acme/core.clj"
          :point-on "another/quux"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "C-c l g r"))
 :then ((:buffer-name "\\`\\*xref\\*\\'")
        (:buffer-matches "src/acme/core\\.clj")))

(provide 'l3-clojure-lsp-navigation)

;;; l3-clojure-lsp-navigation.el ends here
