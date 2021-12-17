(ns aoc-2021.d1
  (:require [clojure.string :as str]))

;;; Day one

(defn sonar-win-n
  "Returns the count of sonar measure increments on input xs for a window of size n.
  The first case corresponds to a window of size 1 and the second case to a window of size 3.
  This implementation uses the fact that a + b + c < b + c + d <=> a < d."
  [xs n]
  (->> (map < xs (nth (iterate rest xs) n))
       (filter true?)
       count))

;;; Day two

(defn product [commands]
  (->> commands
       str/split-lines
       (map (fn [s] (str/split s #" ")))
       (map (fn [[c q]]
              (let [q (Integer. q)]
                (case c
                  "forward" [q 0]
                  "up"      [0 (- 0 q)]
                  "down"    [0 q]
                  [0 0]))))
       (reduce (fn [[ad aq] [d q]] [(+ ad d) (+ aq q)]) [0 0])
       (reduce *)))
