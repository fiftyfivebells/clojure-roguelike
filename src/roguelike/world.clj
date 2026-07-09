(ns roguelike.world
  (:require [roguelike.level :as level]))

(defn new-world
  []
  {:game {:player {:id 0 :type :player :pos [10 12]}
          :current-level (level/test-level)
          :levels        []
          :next-entity-id 1}
   :ui   {:mode {:screen :play}
          :current-msg "Welcome to the dungeon."
          :messages []}})

;; (defn- allocate-entity-id
;;   [world]
;;   (let [next-entity-id (:next-entity-id world)]
;;     (when (= 0 next-entity-id)
;;       (throw (ex-info "entity id 0 is reserved for the player" {:entity-id next-entity-id})))
;;     [next-entity-id (inc next-entity-id)]))

(defn player-entity
  [world]
  (:player (:game world)))

(defn player-pos
  [world]
  (:pos (player-entity world)))

(defn- player?
  [world id]
  (let [player (:player (:game world))]
    (= id (:id player))))

(defn active-actors
  "Gets a list of all active entities in the current level. Essentially just conjs the player onto the list
  of entities for the level."
  [world]
  (conj (level/entities-of (:current-level (:game world))) (:player (:game world))))

(defn get-actor
  [world entity-id]
  (if (player? world entity-id)
    (:player (:game world))
    (level/get-entity (:current-level (:game world)) entity-id)))

(defn update-actor
  [world entity-id f]
  (if (player? world entity-id)
    (update-in world [:game :player] f)
    (update-in world [:game :current-level] level/update-entity entity-id f)))

(defn entity-at
  "Gets the entity at the given coord pair. If the pair is the player's position, return the player. Otherwise,
  get the monster entity's position from the level. Returns nil if there's no entity at the position."
  [world [x y]]
  (let [player (:player (:game world))
        player-pos (:pos player)]
    (if (= player-pos [x y])
      player
      (level/entity-at (:current-level (:game world)) [x y]))))

;; TODO: is this redundant now? check later
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
        [player-x player-y] (:pos (:player game))
        new-x (+ player-x x)
        new-y (+ player-y y)]
    (clamp-position world [new-x new-y])))

(defn move-player
  "Moves the player in the world to the provided coordinates. Returns a new world with the player
  glyph at the new coordinates."
  [world [x y]]
  (let [pos (clamp-position world [x y])
        player-id (:id (:player (:game world)))]
    (update-actor world player-id #(assoc % :pos pos))))

(defn set-mode
  "Updates the world's mode."
  [world mode]
  (assoc-in world [:ui :mode] mode))
