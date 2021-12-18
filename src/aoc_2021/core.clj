(ns aoc-2021.core
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

(defn product [commands]
  (->> (parse commands)
       (reduce (fn [[ad aq] [d q]] [(+ ad d) (+ aq q)]) [0 0])
       (reduce *)))

(defn product2 [commands]
  (->> (parse commands)
       (reduce (fn [[ahoriz adepth aaim] [horiz aim]]
                 (let [caim (+ aaim aim)]
                   [(+ ahoriz horiz) (+ adepth (* caim horiz)) caim]))
               [0 0 0])
       butlast
       (reduce *)))


;;; Day three

(defn- calc [most-common? bnums]
  (let [bnums (str/split-lines bnums)
        ln (-> bnums count)]
    (->> bnums
         (map vec)
         (apply interleave)
         (map str)
         (map #(Integer/parseInt %))
         (partition ln)
         (map #(/ (apply + %) (count %)))
         (map #((if most-common? > <) % 1/2))
         (map #(if % 1 0))
         (reduce #(+ (* 2 %1) %2)))))

(def gamma (partial calc true))
(def epsilon (partial calc false))
(defn power [bnums] (* (gamma bnums) (epsilon bnums)))
