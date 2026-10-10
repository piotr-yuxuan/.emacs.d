;;; l1-treesit-fold-dispatch.el --- treesit-fold dispatch scenarios -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 072 / TF-9, TF-13 to TF-18 (carry-forward index).  Each of the
;; seven user-facing fold bindings is pressed on a Clojure fixture with
;; two top-level `defn' forms, so "fold here, leave that one alone" is
;; asserted in the same `:then' list.
;;
;; The README.org bindings table is the contract:
;;
;;   C-c z z   treesit-fold-toggle
;;   C-c z c   treesit-fold-close
;;   C-c z o   treesit-fold-open
;;   C-c z O   treesit-fold-open-recursively
;;   C-c z a   treesit-fold-open-all
;;   C-c z A   treesit-fold-close-all
;;   C-s-<tab> treesit-fold-toggle (IDE-style alias)
;;
;; treesit-fold folds the BODY of a form, not its header, so the
;; outcomes look at body tokens (`* x x', `+ y y'): folded text is in
;; the buffer but not displayed.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-TS-FOLD-CC-Z-Z: C-c z z folds the defn at point (TF-13)"
 :given ((:name "cc-z-z"
          :file "src/sample.clj"
          :text "(defn-!- f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 :when ((:keys "C-c z z"))
 :then ((:text-hidden "* x x")
        ;; g remains expanded.
        (:text-shown "+ y y")))

(scenario
 :describe "BL1-TS-FOLD-CC-Z-C: C-c z c closes the fold at point (TF-14)"
 :given ((:name "cc-z-c"
          :file "src/sample.clj"
          :text "(defn-!- f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 :when ((:keys "C-c z c"))
 :then ((:text-hidden "* x x")
        (:text-shown "+ y y")))

(scenario
 :describe "BL1-TS-FOLD-CC-Z-O: C-c z o opens a previously folded form (TF-15)"
 :given ((:name "cc-z-o"
          :file "src/sample.clj"
          :text "(defn-!- f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 ;; Fold first, then press the binding under test.
 :when ((:keys "C-c z c") (:keys "C-c z o"))
 :then ((:text-shown "* x x")))

(scenario
 :describe "BL1-TS-FOLD-CC-Z-CAP-O: C-c z O opens recursively (TF-16)"
 :given ((:name "cc-z-cap-o"
          :file "src/sample.clj"
          :text "(defn-!- f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 ;; Close all (two folds), then open recursively at point.
 :when ((:keys "C-c z A") (:keys "C-c z O"))
 :then ((:text-shown "* x x")
        ;; The recursive variant only opens the form at point.
        (:text-hidden "+ y y")))

(scenario
 :describe "BL1-TS-FOLD-CC-Z-A: C-c z a opens every fold in the buffer (TF-17)"
 :given ((:name "cc-z-a"
          :file "src/sample.clj"
          :text "(defn-!- f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 :when ((:keys "C-c z A") (:keys "C-c z a"))
 :then ((:text-shown "* x x")
        (:text-shown "+ y y")))

(scenario
 :describe "BL1-TS-FOLD-CC-Z-CAP-A: C-c z A folds every form in the buffer (TF-18)"
 :given ((:name "cc-z-cap-a"
          :file "src/sample.clj"
          :text "(defn-!- f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 :when ((:keys "C-c z A"))
 :then ((:text-hidden "* x x")
        (:text-hidden "+ y y")))

(scenario
 :describe "BL1-TS-FOLD-CS-TAB: C-s-<tab> is the IDE-style alias for toggle (TF-9)"
 :given ((:name "cs-tab"
          :file "src/sample.clj"
          :text "(defn-!- f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 :when ((:keys "C-s-<tab>"))
 :then ((:text-hidden "* x x")
        (:text-shown "+ y y")))

(provide 'l1-treesit-fold-dispatch)

;;; l1-treesit-fold-dispatch.el ends here
