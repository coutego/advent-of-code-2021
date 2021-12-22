(ns aoc-2021.d04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- read-card [lines card]
  (let [line (str/trim (or (first lines) ""))]
    (if (or (nil? line) (= 0 (count (str/trim line))))
      [(rest lines) card]
      (recur (rest lines) (conj card (map #(Integer/parseInt %) (str/split line #" +")))))))

(defn- read-cards [lines cards]
  (cond
    (= 0 (count lines))
    cards

    (= 0 (count (str/trim (first lines))))
    (recur (rest lines) cards)

    :else
    (let [[lines card] (read-card lines [])
          cards        (conj cards card)]
      (if (= 0 (count lines))
        cards
        (recur lines cards)))))

(defn check-card-rows [card nums]
  (let [line (first card)]
    (if (= 0 (count (clojure.set/difference (set line) (set nums))))
      true
      (if (= 0 (count (rest card)))
        false
        (recur (rest card) nums)))))

(defn check-card-cols [card nums]
  (check-card-rows (apply mapv vector card) nums))

(defn check-card [card nums]
  (or (check-card-rows card nums)
      (check-card-cols card nums)))

(defn find-winner-card [cards nums rest-nums]
  (let [card (->> cards
                  (filter #(check-card % nums))
                  (first))]
    (if card
      [card nums]
      (if rest-nums
        (recur cards (conj nums (first rest-nums)) (rest rest-nums))
        nil))))

(defn score [winner-card nums]
  (-> winner-card
      (->> (apply concat))
      set
      (clojure.set/difference (set nums))
      (->> (reduce +))
      (* (last nums))))

(defn read-input [s]
  (let [lines (str/split-lines s)
        nums  (str/split (first lines) #",")
        nums  (map #(Integer/parseInt %) nums)
        cards (read-cards (drop 2 lines) [])]
    [nums cards]))

(defn p1 [s]
  (let [[nums cards] (read-input s)
        [winner-card rnums] (find-winner-card cards [] nums)]
    (if winner-card
      (score winner-card rnums)
      nil)))

(defn- find-win [card nums]
  (loop [cnums [(first nums)]
         othr   (rest nums)]
    (if (check-card card cnums)
      cnums
      (when (> (count othr) 0)
        (recur (conj cnums (first othr))
               (rest othr))))))

(defn p2 [s]
  (let [[nums cards] (read-input s)]
    (->> cards
         (map #(identity {:card %1 :win (find-win %1 nums)}))
         (filter :win)
         (sort-by #(count (:win %)))
         last
         ((fn [{:keys [card win]}] (score card win))))))
