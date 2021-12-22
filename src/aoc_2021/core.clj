(ns aoc-2021.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn d4-read-input [s]
  (let [lines (str/split-lines s)
        nums  (str/split (first lines) #",")
        nums  (map #(Integer/parseInt %) nums)
        cards (d4-read-cards (drop 2 lines) [])]
    [nums cards]))

(defn d4-p1-main [s]
  (let [[nums cards] (d4-read-input s)
        [winner-card rnums] (d4-find-winner-card cards [] nums)]
    (if winner-card
      (d4-score winner-card rnums)
      nil)))

(defn- d4-find-win [card nums]
  (loop [cnums [(first nums)]
         othr   (rest nums)]
    (if (d4-check-card card cnums)
      cnums
      (when (> (count othr) 0)
        (recur (conj cnums (first othr))
               (rest othr))))))

(defn d4-p2-main [s]
  (let [[nums cards] (d4-read-input s)]
    (->> cards
         (map #(identity {:card %1 :win (d4-find-win %1 nums)}))
         (filter :win)
         (sort-by #(count (:win %)))
         last
         ((fn [{:keys [card win]}] (d4-score card win))))))
