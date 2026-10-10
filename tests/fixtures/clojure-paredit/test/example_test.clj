(ns example-test
  "Plan 062 fixture: trivial kaocha-runnable test file."
  (:require [clojure.test :refer [deftest is]]
            [example]))

(deftest passing-test
  (is (= 2 (example/add))))

(deftest another-passing-test
  (is (= 4 (example/weird-spacing 2))))
