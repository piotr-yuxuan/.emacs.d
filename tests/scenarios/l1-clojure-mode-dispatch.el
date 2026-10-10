;;; l1-clojure-mode-dispatch.el --- Mode dispatch + minor-mode chain for Clojure family -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 062 / first scenario file under the new suite.  Opens
;; `foo.clj' / `foo.cljs' / `foo.cljc' / `foo.bb' / `foo.edn' through
;; `find-file' and asserts the resulting major mode plus the
;; structural-editing minor-mode chain the user expects.
;;
;; The trigger is (:open): the scenario only opens the file.  Opening
;; runs the production hook chain: `auto-mode-alist',
;; `clojure-mode-hook', `paredit-mode', `rainbow-delimiters-mode', and
;; the `lsp-deferred' attached to the mode hook, whose idle timer does
;; not run because the scenario never waits.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-CLJ-DISPATCH-CLJ: opening foo.clj activates clojure-mode + paredit + rainbow-delimiters"
 :given ((:name "clj"
          :file "src/sample.clj"
          :text "(ns sample)\n\n(defn f [x] (* x x))\n"))
 :when ((:open))
 :then ((:mode clojure-mode)
        (:minor-modes-on paredit-mode rainbow-delimiters-mode)))

(scenario
 :describe "BL1-CLJ-DISPATCH-CLJS: opening foo.cljs activates the clojure-mode chain"
 :given ((:name "cljs"
          :file "src/sample.cljs"
          :text "(ns sample)\n\n(defn f [x] (* x x))\n"))
 :when ((:open))
 :then ((:mode clojure-mode)
        (:minor-modes-on paredit-mode rainbow-delimiters-mode)))

(scenario
 :describe "BL1-CLJ-DISPATCH-CLJC: opening foo.cljc activates the clojure-mode chain"
 :given ((:name "cljc"
          :file "src/sample.cljc"
          :text "(ns sample)\n\n(defn f [x] (* x x))\n"))
 :when ((:open))
 :then ((:mode clojure-mode)
        (:minor-modes-on paredit-mode rainbow-delimiters-mode)))

(scenario
 :describe "BL1-CLJ-DISPATCH-BB: opening foo.bb activates the clojure-mode chain (babashka)"
 :given ((:name "bb"
          :file "scripts/sample.bb"
          :text "#!/usr/bin/env bb\n(println :hi)\n")
         ;; Without a shebang only the .bb entry of `auto-mode-alist'
         ;; picks the mode (K07 of the 085 mutation baseline).
         (:name "bb-no-shebang"
          :file "scripts/plain.bb"
          :text "(println :hi)\n"))
 :when ((:open))
 :then ((:mode clojure-mode)))

(scenario
 :describe "BL1-CLJ-DISPATCH-EDN: opening foo.edn activates clojure-mode (edn-mode derives)"
 :given ((:name "edn"
          :file "config/sample.edn"
          :text "{:server {:port 8080}}\n"))
 :when ((:open))
 :then ((:mode clojure-mode)))

(provide 'l1-clojure-mode-dispatch)

;;; l1-clojure-mode-dispatch.el ends here
