(ns aoc-2021.d21)

(defn det-die-create []
  {:rolls 0, :pos 0})

(defn det-die-roll [die]
  (-> die
      (update :rolls inc)
      (update :pos inc)
      (update :pos #(if (> % 100)
                      (- % 100)
                      %))))

(defn play [die]
  (let [d1 (det-die-roll die)
        d2 (det-die-roll d1)
        d3 (det-die-roll d2)]
    {:die d3
     :val (+ (:pos d1) (:pos d2) (:pos d3))}))

(defn next-play [game]
  (let [p (play (:die game))]
    (as-> game g
      (update g :turn inc)
      (assoc g :die (:die p))
      (update-in g
                 [:players (mod (dec (:turn g)) 2)]
                 (fn [pp]
                   (let [pos (inc (mod (dec (+ (:pos pp) (-> p :val))) 10))]
                     {:pos   pos
                      :score (+ (:score pp) pos)}))))))

(defn game [start-a start-b]
  (loop [game {:die     (det-die-create)
               :turn    0
               :players [{:pos   start-a
                          :score 0}
                         {:pos   start-b
                          :score 0}]}]
    (let [players   (:players game)
          max-score (apply max (map :score players))]
      (if (>= max-score 1000)
        (* (-> game :die :rolls) (apply min (map :score players)))
        (recur (next-play game))))))

;;; part 2

(def die-outcomes (for [i (range 3) j (range 3) k (range 3)] [i j k]))

(defn apply-roll [g roll]
  (let [val     (+ 3 (reduce + roll))
        ind     (if (get-in g [0 :last]) 1 0)
        oth-int (if (= ind 0) 1 0)]
    (-> g
        (assoc-in [ind :last] true)
        (assoc-in [oth-int :last] false)
        (update-in [ind :pos] #(inc (mod (dec (+ % val)) 10)))
        (update-in [ind] #(assoc % :score (+ (:score %) (:pos %)))))))

(defn direct-win [g]
  (cond
    (<= 21 (get-in g [0 :score])) [1 0]
    (<= 21 (get-in g [1 :score])) [0 1]
    :else nil))

(defn sum-wins [[a1 a2] [b1 b2]]
  [(+ a1 b1) (+ a2 b2)])

(declare calculate-wins)

(defn calculate-wins- [game]
  (or (direct-win game)
      (reduce
       (fn [acc roll]
         (sum-wins acc (calculate-wins (apply-roll game roll))))
       [0 0]
       die-outcomes)))

(def calculate-wins (memoize calculate-wins-))

(defn game2 [start-a start-b]
  (max (calculate-wins [{:pos start-a :score 0} {:pos start-b :score 0}])))
