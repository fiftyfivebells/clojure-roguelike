(ns roguelike.ai
  (:require [clojure.math :as math]
            [roguelike.rng :as rng]
            [roguelike.world :as world]
            [roguelike.pathfinding :as pathfinding]))

(def ^:private directions
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]        [1  0]
   [-1  1] [0  1] [1  1]])

(defn- wander
  [world]
  (let [make-action (fn [dir] {:action/type :world/move :delta dir})
        actions (map make-action directions)
        [world action] (rng/draw-nth world actions)]
    [world action]))

(defn- chase
  [world monster]
  (let [walkable? (partial world/walkable-at? world)
        field (pathfinding/distance-map walkable? (world/player-pos world))
        monster-pos (:pos monster)
        here-dist (field monster-pos)
        neighbors (pathfinding/neighbors-in-map field monster-pos)]
    (if (empty? neighbors)
      (wander world)
      (let [smallest (apply min-key val neighbors)]
        (if (and here-dist (< (val smallest) here-dist))
          (let [delta (mapv - (key smallest) monster-pos)]
            [world {:action/type :world/move :delta delta}])
          (wander world))))))

(defn decide
  [world actor-id]
  (let [monster    (world/get-actor world actor-id)
        player-pos (world/player-pos world)]
    (if (world/can-see? world actor-id player-pos)
      (chase world monster)
      (wander world))))
