(ns roguelike.world
  (:require [roguelike.level :as level]))

(defn new-world
  []
  {:game {:player        {:x 10 :y 12}
          :current-level (level/test-level)
          :levels        []}
   :ui   {:mode {:screen :play}
          :current-msg "Welcome to the dungeon."
          :messages []}})
assoc-in
;; (defn new-world
;;   []
;;    {:player {:x 10 :y 12}
;;     :width 80
;;     :play-start-row 1
;;     :current-level (level/test-level)
;;     :levels []
;;     :map-rows 22
;;     :msg-row 23
;;     :current-msg "Welcome to the dungeon."
;;     :messages ["Welcome to the dungeon."]
;;     :height 24
;;     :mode {:screen :play}})

(defn clamp-position
  "Takes in a world and a pair of coordinates. Checks against the boundary of the world before returning the
  new coordinates. The return value is either the coordinates that were passed (if they are within the bounds
  of the world) or the edge of the world."
  [world [new-x new-y]]
  (let [game           (:game world)
        [width height] (level/level-dimensions (:current-level game))]
    [(max 0 (min new-x (- width 1)))
     (max 0 (min new-y (- height 1)))]))

(defn add-message
  [world message]
  (-> world
      (assoc-in [:ui :current-msg] message)
      (update-in [:ui :messages] conj message)))

(defn get-tile
  "Takes in a world and coords, then dispatches to the tile-at function in the level namespace using the
  currently active level. Returns a map containing the tile at the coords and the x and y positions."
  [world [x y]]
  (let [game (:game world)
        tile (level/tile-at (:current-level game) [x y])]
    {:tile tile :x x :y y}))

(defn get-proposed-coords
  "Takes in a world and an [x y] delta. Then it calls applies the delta to the player glyph's current
  position and returns the result of clamp-position."
  [world delta]
  (let [[x y] delta
        game (:game world)
        player-pos (:player game)
        new-x (+ (:x player-pos) x)
        new-y (+ (:y player-pos) y)]
    (clamp-position world [new-x new-y])))

(defn move-player
  "Moves the player in the world to the provided coordinates. Returns a new world with the player
  glyph at the new coordinates."
  [world [x y]]
  (let [[new-x new-y] (clamp-position world [x y])]
    (assoc-in world [:game :player] {:x new-x :y new-y})))

(defn set-mode
  "Updates the world's mode."
  [world mode]
  (assoc-in world [:ui :mode] mode))
