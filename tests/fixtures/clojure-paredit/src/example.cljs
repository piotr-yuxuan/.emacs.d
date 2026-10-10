(ns example
  "Plan 062 fixture: ClojureScript variant of `src/example.clj'.

  Same shape, different extension so the L1 dispatch scenario can
  assert that `:cljs' files activate the same `clojure-mode' chain.")

(defn add []
  (+ 1) 2)
