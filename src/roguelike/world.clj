(ns roguelike.world
  (:require [roguelike.ai :as ai]
            [roguelike.level :as level]
            [roguelike.rng :as rng]))

(defn- allocate-entity-id
  [world]
  (let [next-entity-id (:next-entity-id world)]
    ;; this when branch should not be possible to reach, it's a "just in case" safety net
    (when (= 0 next-entity-id)
      (throw (ex-info "entity id 0 is reserved for the player" {:entity-id next-entity-id})))

    [next-entity-id (update world :next-entity-id inc)]))

;; TODO: this is hard-coded and simple for now, but there will eventually be an EDN file with monster
;; templates that this pulls from.
(defn spawn-entity
  [world entity-template]
  (let [[next-id next-world] (allocate-entity-id world)
        monster {:id next-id :glyph \m :type :generic-monster :pos [15 15] :next-time (:current-time next-world)}]
    (update next-world :current-level level/add-entity monster)))

;; the player is intentionally NOT in the per-level entity map, and is instead a global concept.
;; this is so I don't have to move the player in and out of the lists for the different levels.
;; the entire idea behind the "active-actors" function below is to facilitate this.
(defn new-world
  ([]
   (new-world 123456789))  ;; just some default seed
  ([seed]
   (let [world {:player {:id 0 :glyph \@ :type :player :pos [10 12] :next-time 0}
                :current-level (level/test-level)
                :levels        []
                :next-entity-id 1
                :next-tick 10
                :current-time 0
                :rng-state (rng/make seed)}]
     (spawn-entity world ""))))

(defn current-level->tile-list
  "Takes a world and gives back a list of every tile in the current level along with its [x y] position."
  [world]
  (level/level->tile-list (:current-level world)))

(defn player-entity
  [world]
  (:player world))

(defn player-pos
  [world]
  (:pos (player-entity world)))

(defn player?
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

(defn tile-at
  "Takes in a world and coords, then dispatches to the tile-at function in the level namespace using the
  currently active level. Returns a map containing the tile at the coords and the x and y positions."
  [world [x y]]
  (let [tile (level/tile-at (:current-level world) [x y])]
    (assoc tile :pos [x y])))

(defn get-proposed-coords
  "Takes in a world, actor-id, and an [x y] delta. Then it finds the actor using the id and creates proposed
  coords based on the provided delta. Gives back the new coords."
  [world actor-id delta]
  (let [[x y] delta
        actor (get-actor world actor-id)
        [actor-x actor-y] (:pos actor)
        new-x (+ actor-x x)
        new-y (+ actor-y y)]
    [new-x new-y]))

(defn move-actor
  "Moves the actor in the world to the provided coordinates. Returns a new world with the actor
  glyph at the new coordinates."
  [world actor-id [x y]]
  (update-actor world actor-id #(assoc % :pos [x y])))

(defn classify-destination
  "Classifies what's at the given coords for movement purposes: :passable, :blocked-wall,
  :blocked-door, or :blocked-actor. This is the seam that owns tile-based movement
  classification, so callers never need to read a tile's raw :type themselves."
  [world [x y]]
  (if (entity-at world [x y])
    :blocked-actor
    (case (level/classify-tile (level/tile-at (:current-level world) [x y]))
      :floor       :passable
      :open-door   :passable
      :wall        :blocked-wall
      :closed-door :blocked-door
      :blocked-unknown)))

(defn attempt-movement
  [world actor-id delta]
  (let [new-pos (get-proposed-coords world actor-id delta)
        destination (classify-destination world new-pos)]
    (if (= destination :passable)
      [(move-actor world actor-id new-pos) [{:type :moved}]]
      [world [{:type :blocked :by destination}]])))

(defn update-world
  [world actor-id action]
  (case (:type action)
    :world/move (attempt-movement world actor-id (:delta action))
    :world/wait [world []]

    ;; default: throw an exception, because getting here is a mistake that shouldn't happen
    (throw (ex-info "unknown action type" {:action action}))))




