(ns roguelike.ai
  (:require [roguelike.rng :as rng]))

(def ^:private directions
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]        [1  0]
   [-1  1] [0  1] [1  1]])

(defn decide
  [rng-state world actor-id]
  (let [[rng-state val] (rng/rand-int-range rng-state 0 (inc (count directions)))
        action (if (>= val (count directions))
                 {:action/type :world/wait}
                 {:action/type :world/move :delta (nth directions val)})]
    [rng-state action]))
