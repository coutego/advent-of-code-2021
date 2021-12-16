(ns aoc-2021.d1p1
  (:gen-class))

(defn sonar-reducer [{:keys [curr cnt]} n]
  {:curr n :cnt (if (> n curr)
                  (inc cnt)
                  cnt)})

(defn sonar [xs]
  (-> (reduce sonar-reducer
              {:curr (apply max xs)
               :cnt 0}
              xs)
      :cnt))

(defn sliding-sonar [xs]
  (let [x1 xs
        x2 (-> (drop 1 xs) vec (conj 0))
        x3 (-> (drop 2 xs) vec (conj 0) (conj 0))]
    (->> [x1 x2 x3]
         interleave
         (partition 3))))

(defn -main
  "Apply sonar funtion to input"
  [& args]
  (println "Sonar for given input: " (apply sonar args)))
