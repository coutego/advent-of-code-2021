(ns aoc-2021.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
         (map #((if most-common? > <=) % 1/2))
         (map #(if % 1 0))
         (reduce #(+ (* 2 %1) %2)))))

(def gamma (partial calc true))
(def epsilon (partial calc false))
(defn power [bnums] (* (gamma bnums) (epsilon bnums)))

(defn- retain [most-common? ind bnums]
  (let [it (group-by #(nth % ind) bnums)
        it (if ((if most-common? < >=)
                (count (it \1))
                (count (it \0)))
             (it \1)
             (it \0))]
    (if (= 1 (count it))
      it
      (recur most-common? (inc ind) it))))

(defn bstr2num [s]
  (reduce #(+ (* 2 %1) (Integer/parseInt (str %2))) 0 s))

(def oxigen-r (comp bstr2num first (partial #'retain true 0) str/split-lines))
(def co2-sc-r (comp bstr2num first (partial #'retain false 0) str/split-lines))

(defn life-support-rating [bnums]
  (* (oxigen-r bnums) (co2-sc-r bnums)))

;;; Day 4

(defn- d4-read-card [lines card]
  (let [line (str/trim (or (first lines) ""))]
    (if (or (nil? line) (= 0 (count (str/trim line))))
      [(rest lines) card]
      (recur (rest lines) (conj card (map #(Integer/parseInt %) (str/split line #" +")))))))

(defn- d4-read-cards [lines cards]
  (cond
    (= 0 (count lines))
    cards

    (= 0 (count (str/trim (first lines))))
    (recur (rest lines) cards)

    :else
    (let [[lines card] (d4-read-card lines [])
          cards        (conj cards card)]
      (if (= 0 (count lines))
        cards
        (recur lines cards)))))

(defn d4-check-card-rows [card nums]
  (let [line (first card)]
    (if (= 0 (count (clojure.set/difference (set line) (set nums))))
      true
      (if (= 0 (count (rest card)))
        false
        (recur (rest card) nums)))))

(defn d4-check-card-cols [card nums]
  (d4-check-card-rows (apply mapv vector card) nums))

(defn d4-check-card [card nums]
  (or (d4-check-card-rows card nums)
      (d4-check-card-cols card nums)))

(defn d4-find-winner-card [cards nums rest-nums]
  (let [card (->> cards
                  (filter #(d4-check-card % nums))
                  (first))]
    (if card
      [card nums]
      (if rest-nums
        (recur cards (conj nums (first rest-nums)) (rest rest-nums))
        nil))))

(defn d4-score [winner-card nums]
  (-> winner-card
      (->> (apply concat))
      set
      (clojure.set/difference (set nums))
      (->> (reduce +))
      (* (last nums))))

(defn d4-p1-main [s]
  (let [lines (str/split-lines s)
        nums  (str/split (first lines) #",")
        nums  (map #(Integer/parseInt %) nums)
        cards (d4-read-cards (drop 2 lines) [])
        [winner-card rnums] (d4-find-winner-card cards [] nums)]
    (if winner-card
      (d4-score winner-card rnums)
      nil)))
