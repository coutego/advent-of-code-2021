(ns aoc-2021.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
