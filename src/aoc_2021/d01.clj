(ns aoc-2021.d01
  (:require [clojure.string :as st]))

(defn sonar-win-n
  "Returns the count of sonar measure increments on input xs for a window of size n.
  The first case corresponds to a window of size 1 and the second case to a window of size 3.
  This implementation uses the fact that a + b + c < b + c + d <=> a < d."
  [xs n]
  (->> (map < xs (nth (iterate rest xs) n))
       (filter true?)
       count))

(defn p1 [lines] (sonar-win-n (st/split-lines lines) 1))
(defn p2 [lines] (sonar-win-n (st/split-lines lines) 3))
