(ns acme.core
  "Fixture entry point for plan 049 Clojure-navigation integration tests.

  References used by tests/test-integration-clojure-nav.sh:
   - acme.another/helper          → CN-1 (project-local navigation)
   - clojure.core/map             → CN-2 (Clojure stdlib)
   - malli.core/schema            → CN-3 (third-party Clojure library)
   - java.util.ArrayList          → CN-4 (Java type with sources)
   - org.apache.commons.lang3.StringUtils → CN-5 (Java type, sources may
     be missing; exercises clojure-lsp's source-download path)
   - clojure.lang.PersistentList  → CN-6 (Java class with no published
     source jar; exercises clojure-lsp's CFR decompiler fallback)"
  (:require [acme.another :as another]
            [clojure.core :as core]
            [malli.core :as m])
  (:import (java.util ArrayList)
           (org.apache.commons.lang3 StringUtils)
           (clojure.lang PersistentList)))

(defn use-helper
  "Calls into acme.another."
  [x]
  (another/helper x))

(defn use-quux
  "Plan 062b: cross-namespace call site used by M-? scenarios."
  [x]
  (another/quux x))

(def schema (m/schema :int))

(defn use-stdlib [xs] (core/map inc xs))

(defn use-java-list []
  (let [xs (ArrayList.)]
    (.add xs 1)
    xs))

(defn use-stringutils [s] (StringUtils/reverse s))

(defn empty-list [] PersistentList/EMPTY)
