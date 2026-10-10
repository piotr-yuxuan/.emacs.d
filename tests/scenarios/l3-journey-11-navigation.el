;;; l3-journey-11-navigation.el --- Journey 11: definitions and references land on their line -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, journey 11.  With a real language server (BasedPyright
;; for Python, clojure-lsp for Clojure), M-. on a call lands on the
;; line of the definition in the other file, and the references of a
;; definition list its call site, where <return> lands.  The user
;; picks the call site in the *xref* buffer with C-s.  Vanilla M-?
;; asks which identifier, with the one at point as the default.
;;
;; In Clojure buffers M-? is paredit's `paredit-convolute-sexp':
;; paredit's own keys stay in paredit buffers (decision D4), so the
;; Clojure references go through lsp-mode's C-c l g r.
;;
;; The mutation binds M-. (or M-?) to a command that does nothing:
;; point then stays where it was.  Each row adds its project to the
;; private lsp session and waits, as an idle user, for the server to
;; index it; a missing server skips the row (tests/expected-skips/094.el).
;;
;; The journey is about where point lands, not about latency: the full
;; profile runs many language servers at once, and a cold BasedPyright
;; then answers its first requests after lsp-mode's 10 s default.  This
;; file's session waits up to 60 s for a response.

;;; Code:

(require 'scenario)

(setq lsp-response-timeout 60)

(scenario
 :describe "J-11: M-. in Python lands on the definition"
 :given ((:name "python" :project "journey-python" :entry "main.py"
          :point-on "shout(" :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-indexed))
 :when ((:keys "M-."))
 :then ((:file-suffix "/journey-python/helpers.py")
        (:line-at-point "\\`def shout(text: str) -> str:\\'")))

(scenario
 :describe "J-11: M-? in Python lands on the call site"
 :given ((:name "python" :project "journey-python" :entry "helpers.py"
          :point-on "shout" :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-indexed))
 :when ((:keys "M-? <return>") (:sample "references")
        (:keys "C-s") (:type "helpers.shout") (:keys "<return> <return>"))
 :then ((:sampled "references" :buffer "\\`\\*xref\\*\\'")
        (:file-suffix "/journey-python/main.py")
        (:line-at-point "\\`print(helpers\\.shout(\"hi\"))\\'")))

(scenario
 :describe "J-11: M-. in Clojure lands on the definition"
 :given ((:name "clojure" :project "journey-clojure" :entry "src/journey/core.clj"
          :point-on "util/shout" :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "M-."))
 :then ((:file-suffix "/journey-clojure/src/journey/util.clj")
        (:line-at-point "\\`(defn shout\\'")))

(scenario
 :describe "J-11: C-c l g r in Clojure lands on the call site"
 :given ((:name "clojure" :project "journey-clojure" :entry "src/journey/util.clj"
          :point-on "shout" :system (:lsp-root ".")))
 :wait-for ((:lsp-attached clojure-lsp) (:lsp-indexed))
 :when ((:keys "C-c l g r") (:sample "references")
        (:keys "C-s") (:type "util/shout") (:keys "<return> <return>"))
 :then ((:sampled "references" :buffer "\\`\\*xref\\*\\'")
        (:file-suffix "/journey-clojure/src/journey/core.clj")
        (:line-at-point "\\`  (util/shout someone))\\'")))

(provide 'l3-journey-11-navigation)

;;; l3-journey-11-navigation.el ends here
