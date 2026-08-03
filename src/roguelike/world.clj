(ns roguelike.world
  (:require [roguelike.knowledge :as knowledge]
            [roguelike.level :as level]
            [roguelike.rng :as rng]
            [roguelike.fov :as fov]
            [roguelike.dungeon :as dungeon]))

;; Levels

(defn- current-level-path
  [world]
  [:levels (:current-level-id world)])

(defn current-level
  "Returns the currently active level of the world."
  [world]
  (get-in world (current-level-path world)))

(defn update-current-level
  [world f]
  (update-in world (current-level-path world) f))

;; World Construction

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
  [world]
  (let [[next-id next-world] (allocate-entity-id world)
        monster {:entity/id next-id
                 :entity/type :generic-monster
                 :pos (level/room-center (second (level/get-rooms (current-level world))))
                 :sight/radius 2  ;; TODO: just a random magic number, fix to be more robust later
                 :next-time (:current-time next-world)}]
    (update-current-level next-world #(level/add-entity % monster))))

;; Actors

(defn entity-type
  [entity]
  (:entity/type entity))

(defn player-entity
  [world]
  (:player world))

(defn player-id
  [world]
  (:entity/id (player-entity world)))

(defn player-pos
  [world]
  (:pos (player-entity world)))

(defn player?
  [world id]
  (let [player (:player world)]
    (= id (:entity/id player))))

(defn active-actors
  "Gets a list of all active entities in the current level. Essentially just conjs the player onto the list
  of entities for the level."
  [world]
  (conj (level/entities-of (current-level world)) (:player world)))

(defn get-actor
  [world entity-id]
  (if (player? world entity-id)
    (:player world)
    (level/get-entity (current-level world) entity-id)))

(defn update-actor
  [world entity-id f]
  (if (player? world entity-id)
    (update world :player f)
    (update-current-level world #(level/update-entity % entity-id f))))

(defn entity-at
  "Gets the entity at the given coord pair. If the pair is the player's position, return the player. Otherwise,
  get the monster entity's position from the level. Returns nil if there's no entity at the position."
  [world [x y]]
  (let [player (:player world)
        player-pos (:pos player)]
    (if (= player-pos [x y])
      player
      (level/entity-at (current-level world) [x y]))))

;; Tiles

(defn tile-at
  "Takes in a world and coords, then dispatches to the tile-at function in the level namespace using the
  currently active level. Returns a map containing the tile at the coords and the x and y positions."
  [world [x y]]
  (let [tile (level/tile-at (current-level world) [x y])]
    (assoc tile :pos [x y])))

;; FOV

(defn sight-radius
  "Returns the sight-radius for actor with the provided actor-id."
  [world actor-id]
  (let [actor (get-actor world actor-id)]
    (:sight/radius actor)))

(defn visible-cells
  "Takes a world and sight-radius and returns a set of tiles visible from the "
  [world actor-id radius]
  (let [opaque? (partial level/opaque-at? (current-level world))
        actor-pos (:pos (get-actor world actor-id))]
    (fov/visible-cells opaque? actor-pos radius)))

(defn observe
  [world]
  (let [radius (sight-radius world (player-id world))
        visible (visible-cells world (player-id world) radius)]
    (update-current-level world #(level/remember-visible % visible))))

(defn can-see?
  [world from-id to-pos]
  (let [radius (sight-radius world from-id)
        opaque? (partial level/opaque-at? (current-level world))
        visible (visible-cells world from-id radius)]
    (contains? visible to-pos)))

(defn visible-actors
  "Gets a list of active entities in the current level whose position is currently visible to the
  player (per FOV), so monsters outside the player's sight radius don't render."
  [world]
  (let [visible (visible-cells world (player-id world) (sight-radius world (player-id world)))]
    (filter #(contains? visible (:pos %)) (active-actors world))))

(defn level-view
  [world]
  (let [visible (visible-cells world (player-id world) (sight-radius world (player-id world)))
        curr-lvl (current-level world)
        known   (level/known-tiles curr-lvl)
        classifier (fn [tile]
                     (let [pos (:pos tile)]
                       (cond
                         (contains? visible pos)
                         {:pos pos
                          :state :visible
                          :tile (level/classify-tile (level/tile-at curr-lvl pos))}

                         (knowledge/seen? known pos)
                         {:pos pos
                          :state :remembered
                          :tile (level/classify-tile (knowledge/remembered-tile known pos))}

                         :else
                         {:pos pos
                          :state :unknown
                          :tile :unknown})))]
    (map classifier (level/level->tile-list curr-lvl))))

;; Movement

(defn walkable-at?
  [world [x y]]
  (level/walkable-at? (current-level world) [x y]))

(defn get-proposed-coords
  "Takes in a world, actor-id, and an [x y] delta. Then it finds the actor using the id and creates
  proposed coords based on the provided delta. Gives back the new coords."
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
  "Classifies what's at the given coords for movement purposes: :passable, :wall,
  :door, or :actor. The default case returns :unknown."
  [world [x y]]
  (cond
    (entity-at world [x y]) :actor
    (level/walkable-at? (current-level world) [x y]) :passable
    ;; Only reached when movement is already blocked: this case exists solely to
    ;; name the obstacle for the player-facing message.
    :else (case (level/classify-tile (tile-at world [x y]))
            :wall        :wall
            :closed-door :door
            :unknown)))

(defn attempt-movement
  [world actor-id delta]
  (let [new-pos (get-proposed-coords world actor-id delta)
        destination (classify-destination world new-pos)
        player? (player? world actor-id)]
    (if (= destination :passable)
      [(move-actor world actor-id new-pos) [{:event/type :world/moved :player? player?}]]
      [world [{:event/type :world/blocked :by destination :player? player?}]])))

(defn- next-level-id
  [world]
  (inc (:current-level-id world)))

(defn- previous-level-id
  [world]
  (dec (:current-level-id world)))

(defn- change-current-level
  [world id lvl]
  (-> world
      (assoc :current-level-id id)
      (assoc-in [:levels id] lvl)))

(defn descend
  [world]
  (let [next-lvl-id (next-level-id world)
        ;; levels already visited stay in :levels, so only generate on first arrival
        next-lvl (or (get-in world [:levels next-lvl-id])
                     (dungeon/generate (:world-seed world) next-lvl-id true))]
    [(-> world
         (change-current-level next-lvl-id next-lvl)
         (move-actor (player-id world) (:up next-lvl)))
     []]))

(defn ascend
  [world]
  (let [prev-lvl-id (previous-level-id world)
        prev-lvl (get-in world [:levels prev-lvl-id])]
    [(-> world
         (change-current-level prev-lvl-id prev-lvl)
         (move-actor (player-id world) (:down prev-lvl)))
     []]))

(defn- on-stairs-down?
  [world]
  (level/on-stairs-down? (current-level world) (player-pos world)))

(defn- on-stairs-up?
  [world]
  (level/on-stairs-up? (current-level world) (player-pos world)))

(defn update-world
  [world actor-id action]
  (case (:action/type action)
    :world/descend (if (on-stairs-down? world)
                     (descend world)
                     [world []])
    :world/ascend (if (on-stairs-up? world)
                    (ascend world)
                    [world []])
    :world/move (attempt-movement world actor-id (:delta action))
    :world/wait [world []]

    ;; default: throw an exception, because getting here is a mistake that shouldn't happen
    (throw (ex-info "unknown action type" {:action action}))))

;; the player is intentionally NOT in the per-level entity map, and is instead a global concept.
;; this is so I don't have to move the player in and out of the lists for the different levels.
;; the entire idea behind the "active-actors" function below is to facilitate this.
(defn new-world
  ([]
   (new-world 123456789))  ;; just some default seed
  ([seed]
   (let [first-dungeon (dungeon/generate seed 0 false)
         player-start-pos (level/room-center (first (level/get-rooms first-dungeon)))
         world {:world-seed seed
                :player {:entity/id 0
                         :entity/type :player
                         :pos player-start-pos
                         :sight/radius 4  ;; TODO: just a random magic number, fix to be more robust later
                         :next-time 0}
                :current-level-id 0
                :levels {0 first-dungeon}
                :next-entity-id 1
                :next-tick 10
                :current-time 0
                :rng-state (rng/make seed)}]
     (-> world
         (spawn-entity)
         (observe)))))
