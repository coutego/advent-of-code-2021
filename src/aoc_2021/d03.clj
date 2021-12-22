(ns aoc-2021.d03
  (:require [clojure.string :as str]))

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

(def p1 power)
(def p2 life-support-rating)
