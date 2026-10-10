(ns acme.another
  "Companion namespace called from acme.core for plan 049 CN-1.")

(defn helper
  "A trivial function called from acme.core/use-helper."
  [x]
  (str "helper-" x))

(defn quux
  "Defn target used by plan 062b cross-namespace navigation scenarios."
  [x]
  (inc x))
