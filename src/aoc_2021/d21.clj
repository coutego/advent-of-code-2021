(ns aoc-2021.d21)

(defprotocol IDie
  (roll [this])
  (pos [this]))

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
