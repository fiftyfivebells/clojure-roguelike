(ns roguelike.world
  (:require [roguelike.level :as level]))

(defn new-world
  []
   {:player {:x 10 :y 12}
    :width 80
    :play-start-row 1
    :current-level (level/test-level)
    :levels []
    :map-rows 22
    :msg-row 23
    :current-msg "Welcome to the dungeon."
    :messages ["Welcome to the dungeon."]
    :height 24
    :mode {:screen :play}})

(defn clamp-position
  [world {new-x :x new-y :y}]
  {:x (max 0 (min new-x (- (:width world) 1)))
   :y (max 0 (min new-y (- (:map-rows world) 1)))})

(defn add-message
  [world message]
  (-> world
      (assoc :current-msg message)
      (update :messages conj message)))

(defn get-tile
  [world [x y]]
  (let [tile (level/tile-at (:current-level world) [x y])]
    {:tile tile :x x :y y}))

(defn get-proposed-coords
  [world delta]
  (let [[x y] delta
        player-pos (:player world)
        new-x (+ (:x player-pos) x)
        new-y (+ (:y player-pos) y)
        new-pos (clamp-position world {:x new-x :y new-y})]
    [(:x new-pos) (:y new-pos)]))

(defn move-player
  [world [x y]]
  (let [new-pos (clamp-position world {:x x :y y})]
    (assoc world :player new-pos)))

(defn set-mode
  [world mode]
  (assoc world :mode mode))
