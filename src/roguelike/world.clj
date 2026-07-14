(ns roguelike.world
  (:require [roguelike.level :as level]
            [roguelike.rng :as rng]))

;; the player is intentionally NOT in the per-level entity map, and is instead a global concept.
;; this is so I don't have to move the player in and out of the lists for the different levels.
;; the entire idea behind the "active-actors" function below is to facilitate this.
(defn new-world
  ([]
   (new-world 123456789))  ;; just some default seed
  ([seed]
   {:player {:id 0 :type :player :pos [10 12]}
    :current-level (level/test-level)
    :levels        []
    :next-entity-id 1
    :rng-state (rng/make seed)}))

;; (defn- allocate-entity-id
;;   [world]
;;   (let [next-entity-id (:next-entity-id world)]
;;     (when (= 0 next-entity-id)
;;       (throw (ex-info "entity id 0 is reserved for the player" {:entity-id next-entity-id})))
;;     [next-entity-id (inc next-entity-id)]))

(defn player-entity
  [world]
  (:player world))

(defn player-pos
  [world]
  (:pos (player-entity world)))

(defn- player?
  [world id]
  (let [player (:player world)]
    (= id (:id player))))

(defn active-actors
  "Gets a list of all active entities in the current level. Essentially just conjs the player onto the list
  of entities for the level."
  [world]
  (conj (level/entities-of (:current-level world)) (:player world)))

(defn get-actor
  [world entity-id]
  (if (player? world entity-id)
    (:player world)
    (level/get-entity (:current-level world) entity-id)))

(defn update-actor
  [world entity-id f]
  (if (player? world entity-id)
    (update world :player f)
    (update world :current-level level/update-entity entity-id f)))

(defn entity-at
  "Gets the entity at the given coord pair. If the pair is the player's position, return the player. Otherwise,
  get the monster entity's position from the level. Returns nil if there's no entity at the position."
  [world [x y]]
  (let [player (:player world)
        player-pos (:pos player)]
    (if (= player-pos [x y])
      player
      (level/entity-at (:current-level world) [x y]))))

(defn get-tile
  "Takes in a world and coords, then dispatches to the tile-at function in the level namespace using the
  currently active level. Returns a map containing the tile at the coords and the x and y positions."
  [world [x y]]
  (let [tile (level/tile-at (:current-level world) [x y])]
    {:tile tile :x x :y y}))

(defn get-proposed-coords
  "Takes in a world and an [x y] delta. Then it calls applies the delta to the player glyph's current
  position and returns the result of clamp-position."
  [world delta]
  (let [[x y] delta
        [player-x player-y] (:pos (:player world))
        new-x (+ player-x x)
        new-y (+ player-y y)]
    [new-x new-y]))

(defn move-player
  "Moves the player in the world to the provided coordinates. Returns a new world with the player
  glyph at the new coordinates."
  [world [x y]]
  (let [player-id (:id (:player world))]
    (update-actor world player-id #(assoc % :pos [x y]))))

(defn attempt-movement
  [world delta]
  (let [new-pos (get-proposed-coords world delta)
        tile (get-tile world new-pos)]
    (if (level/is-passable? (:tile tile))
      [(move-player world new-pos) {:type :moved}]
      [world {:type :hit-impassable}])))

(defn update-world
  [world action]
  (let [action-type (name (:type action))]
    (case action-type
      "move" (attempt-movement world [(:dx action) (:dy action)])
      "none" [world nil])))

