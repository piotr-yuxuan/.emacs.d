;;; l3-key-scheme.el --- One meaning per key, with real programs (plan 093) -*- lexical-binding: t -*-
;; scenario-tier: l3
;; scenario-display: graphic

;;; Commentary:
;; Plan 093, decision D4, for the keys whose outcome needs a real
;; program or a fresh session; see l1-key-scheme.el for the others.
;;
;; The Java scenarios come first, so that dape is not loaded yet: the
;; debugger keys must exist before their first use loads dape (084
;; CF-15).  The first one checks that precondition, which the second,
;; pressing the keys, cannot check before them.  The Java file's
;; language server never starts, as the keys leave no idle time.

;;; Code:

(require 'scenario)

(scenario
 :describe "CF-15: dape is not loaded when a Java file opens"
 :given ((:name "java" :file "src/Main.java" :text "class Main {}-!-\n"))
 :when ((:open))
 :then ((:variable-excludes features dape)))

(scenario
 :describe "CF-15: C-c d d starts the debugger in Java before dape has loaded"
 :given ((:name "java" :file "src/Main.java" :text "class Main {}-!-\n"))
 :when ((:keys "C-c d d") (:sample "adapter") (:keys "C-g"))
 :then ((:mode java-ts-mode)
        (:sampled "adapter" :minibuffer "\\`Run adapter: ")))

(provide 'l3-key-scheme)

;;; l3-key-scheme.el ends here
