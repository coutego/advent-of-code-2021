(ns aoc-2021.d02
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [commands]
  (->> commands
       str/split-lines
       (map (fn [s] (str/split s #" ")))
       (map (fn [[c q]]
              (let [q (Integer. q)]
                (case c
                  "forward" [q 0]
                  "up"      [0 (- 0 q)]
                  "down"    [0 q]
                  [0 0]))))))

(defn p1 [commands]
  (->> (parse commands)
       (reduce (fn [[ad aq] [d q]] [(+ ad d) (+ aq q)]) [0 0])
       (reduce *)))

(defn p2 [commands]
  (->> (parse commands)
       (reduce (fn [[ahoriz adepth aaim] [horiz aim]]
                 (let [caim (+ aaim aim)]
                   [(+ ahoriz horiz) (+ adepth (* caim horiz)) caim]))
               [0 0 0])
       butlast
       (reduce *)))
