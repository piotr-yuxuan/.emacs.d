;;; l1-treesit-fold-lifecycle.el --- treesit-fold lifecycle on a Clojure file -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 072 / TF-7 (carry-forward index).  Opening a Clojure file
;; triggers `my/treesit-fold-bootstrap' (attached to
;; `emacs-startup-hook' by `init.el'), which wires
;; `my/treesit-fold-ensure-parser' to `after-change-major-mode-hook'
;; so non-`-ts-mode' Clojure buffers get a tree-sitter parser when the
;; grammar is installed.
;;
;; TF-7 asserts that `treesit-fold-mode' is on in the Clojure buffer;
;; the parser-creation precondition (TF-8) is exercised by the
;; dispatch scenarios of `l1-treesit-fold-dispatch.el', since a fold
;; only succeeds when the parser exists.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-TS-FOLD-LIFECYCLE-CLJ-MODE: opening sample.clj activates treesit-fold-mode (TF-7, TF-8)"
 :given ((:name "clj-fold-mode"
          :file "src/sample.clj"
          :text "(defn f [x]\n  (* x x))\n\n(defn g [y]\n  (+ y y))\n"))
 :when ((:open))
 :then ((:mode clojure-mode)
        (:minor-modes-on treesit-fold-mode)))

(provide 'l1-treesit-fold-lifecycle)

;;; l1-treesit-fold-lifecycle.el ends here
