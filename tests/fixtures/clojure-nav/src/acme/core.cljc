(ns acme.core
  "Plan 062b cljc variant of acme.core.

  This file mirrors src/acme/core.clj for the cross-namespace
  navigation scenarios.  The point of the cljc variant is that
  M-. (`lsp-find-definition') from a `.cljc' buffer must land on the
  same target as from a `.clj' buffer; if a future `clojure-mode'
  release stops attaching `lsp-deferred' to `clojurec-mode-hook',
  the BL3-CLJ-LSP-FIND-DEFINITION-CLJC scenario fails loudly.

  Java imports from core.clj are intentionally omitted: cljc can run
  on both the JVM and ClojureScript, so the file stays JVM-host
  agnostic.  Only the project-local require survives, which is all
  the cross-namespace M-. probe needs."
  (:require [acme.another :as another]))

(defn use-helper
  "Calls into acme.another (clj parity)."
  [x]
  (another/helper x))

(defn use-quux
  "Plan 062b: cross-namespace call site used by M-? scenarios."
  [x]
  (another/quux x))
