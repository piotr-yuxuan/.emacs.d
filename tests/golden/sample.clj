(ns sample.inventory
  "A golden fixture of Clojure: a small inventory namespace."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; At most sixteen items per shelf.
(def ^:private limit 16)

(defrecord Item [name price tags])

(defn parse-line
  "Return the item of a NAME,PRICE line, or nil for a comment."
  [line]
  (when-not (str/starts-with? line "#")
    (let [[item-name price] (str/split line #",")]
      (->Item (str/trim item-name) (parse-double price) #{}))))

(defn load-items
  "Read the items of the file at PATH, cheapest first."
  [path]
  (with-open [reader (io/reader path)]
    (->> (line-seq reader)
         (keep parse-line)
         (sort-by (juxt :price :name))
         (take limit)
         doall)))

(defmulti label :currency)

(defmethod label :default
  [{:keys [name price]}]
  (format "%s: %.2f EUR" name price))

(comment
  (load-items "items.csv")
  @(future (count (load-items "items.csv")))
  (label {:name "tea" :price 2.5 :currency nil \x 1}))
