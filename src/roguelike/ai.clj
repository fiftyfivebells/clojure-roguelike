(ns roguelike.ai
  (:require [clojure.math :as math]
            [roguelike.rng :as rng]
            [roguelike.world :as world]
            [roguelike.pathfinding :as pathfinding]
            [roguelike.vision :as vision]))

(def ^:private directions
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]        [1  0]
   [-1  1] [0  1] [1  1]])

(defn- wander
  [world]
  (let [make-action (fn [dir] {:action/type :world/move :delta dir})
        actions (map make-action directions)
        [world action] (rng/draw-any world actions)]
    [world action]))

(defn- chase-map
  [world]
  (let [k [(world/current-level world) (world/player-pos world)]
        cached (:chase-map world)]
    (if (= k (:key cached))
      [world (:field cached)]
      (let [field (pathfinding/distance-map (partial world/walkable-at? world)
                                            (world/player-pos world))]
        [(assoc world :chase-map {:key k :field field}) field]))))

(defn- chase
  [world monster]
  (let [[world field] (chase-map world)
        monster-pos (:pos monster)
        here-dist (field monster-pos)
        neighbors (pathfinding/neighbors-in-map field monster-pos)]
    (if (empty? neighbors)
      (wander world)
      ;; neighbors is a map of pos -> distance from origin, so something like
      ;; {[1 1] 2, [1 2] 3 [1 3] 4}
      ;; which means smallest is a vector: [[1 1] 2]
      ;; (val smallest) -> 2
      ;; (key smallest) -> [1 1]
      (let [smallest (apply min-key val neighbors)]
        (if (and here-dist (< (val smallest) here-dist))
          [world {:action/type :world/move
                  :delta (mapv - (key smallest) monster-pos)}]
          (wander world))))))

(defn decide
  [world actor-id]
  (let [monster    (world/get-actor world actor-id)
        radius     (world/sight-radius world actor-id)]
    (if (vision/sees? (:vision world) (:pos monster) radius)
      (chase world monster)
      (wander world))))
