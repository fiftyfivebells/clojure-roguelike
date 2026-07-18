(ns roguelike.ai
  (:require [roguelike.rng :as rng]))

(def ^:private directions
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]        [1  0]
   [-1  1] [0  1] [1  1]])

(defn decide
  [world actor-id]
  (let [make-action (fn [dir] {:action/type :world/move :delta dir})
        actions (map make-action directions)
        [world action] (rng/draw-nth world actions)]
    [world action]))
