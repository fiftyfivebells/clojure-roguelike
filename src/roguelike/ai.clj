(ns roguelike.ai
  (:require [clojure.math :as math]
            [roguelike.rng :as rng]
            [roguelike.world :as world]))

(def ^:private directions
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]        [1  0]
   [-1  1] [0  1] [1  1]])

(defn- step-toward
  [[mx my] [px py]]
  [(long (math/signum (- px mx))) (long (math/signum (- py my)))])

(defn- wander
  [world]
  (let [make-action (fn [dir] {:action/type :world/move :delta dir})
        actions (map make-action directions)
        [world action] (rng/draw-nth world actions)]
    [world action]))

(defn- chase
  [world monster player-pos]
  [world {:action/type :world/move :delta (step-toward (:pos monster) player-pos)}])

(defn decide
  [world actor-id]
  (let [monster    (world/get-actor world actor-id)
        player-pos (world/player-pos world)]
    (if (world/can-see? world actor-id player-pos)
      (chase world monster player-pos)
      (wander world))))

;; (defn decide
;;   [world actor-id]
;;   (let [make-action (fn [dir] {:action/type :world/move :delta dir})
;;         actions (map make-action directions)
;;         [world action] (rng/draw-nth world actions)]
;;     [world action]))
