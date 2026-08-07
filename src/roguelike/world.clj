(ns roguelike.world
  (:require [roguelike.knowledge :as knowledge]
            [roguelike.level :as level]
            [roguelike.rng :as rng]
            [roguelike.fov :as fov]
            [roguelike.spawn :as spawn]
            [roguelike.dungeon :as dungeon]))

;; Levels

(defn- level-at
  "Takes a world and id and returns the level associated with that id. This is
   intentionally allowed to return nil, so that callers can use the nil as a
   signal that a new level should be created using the provided id."
  [world id]
  (get-in world [:levels id]))

(defn- put-level
  [world id lvl]
  (assoc-in world [:levels id] lvl))

(defn- current-level-id
  [world]
  (:current-level-id world))

(defn- set-current-level-id
  [world id]
  (assoc world :current-level-id id))

(defn- next-level-id
  [world]
  (inc (current-level-id world)))

(defn- previous-level-id
  [world]
  (dec (current-level-id world)))

(defn current-level
  "Returns the currently active level of the world."
  [world]
  (level-at world (current-level-id world)))

(defn update-current-level
  [world f]
  (let [id (current-level-id world)]
    (put-level world id (f (level-at world id)))))

(defn current-level-dimensions
  [world]
  (level/dimensions (current-level world)))

;; Time

(defn- current-time
  [world]
  (:current-time world))

(defn- rebase-monster-clocks
  [world]
  (update-current-level
   world
   (fn [lvl]
     (let [departed (or (level/departed-at lvl) (current-time world))]
       (level/rebase-entity-time lvl (- (current-time world) departed))))))

;; World Construction

(defn- allocate-entity-id
  [world]
  (let [next-entity-id (:next-entity-id world)]
    ;; this when branch should not be possible to reach, it's a "just in case" safety net
    (when (= 0 next-entity-id)
      (throw (ex-info "entity id 0 is reserved for the player" {:entity-id next-entity-id})))

    [next-entity-id (update world :next-entity-id inc)]))

(defn- populate-level
  "Spawns this level's monsters into lvl, giving each one an id from the world's
   counter and a clock set to now. Reserved positions are left empty so the
   player never lands on top of a monster. The spawn rng is left on the level so
   later waves of monsters pick up where this one stopped. Returns [world lvl]."
  [world lvl lvl-id reserved]
  (let [[ctx monsters] (spawn/monsters-for-level (:world-seed world) lvl-id lvl reserved)
        [world lvl] (reduce (fn [[world lvl] monster]
                              (let [[id world] (allocate-entity-id world)]
                                [world (level/add-entity lvl (assoc monster
                                                                    :entity/id id
                                                                    :next-time (current-time world)))]))
                            [world lvl]
                            monsters)]
    [world (level/set-rng-state lvl (:rng-state ctx))]))

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

(defn tile-classifier
  ([world]
   (let [visible (visible-cells world (player-id world) (sight-radius world (player-id world)))
         curr-lvl (current-level world)
         known   (level/known-tiles curr-lvl)]
     (fn [tile]
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
            :tile :unknown}))))))

(defn level-view
  ([world]
   (map (tile-classifier world) (level/level->tile-list (current-level world))))
  ([world origin [vw-max vh-max]]
   (map (tile-classifier world)
        (level/level->tile-list
         (current-level world) origin [vw-max vh-max]))))

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

(defn- change-current-level
  [world id lvl]
  (-> world
      (set-current-level-id id)
      (put-level id lvl)))

(defn- set-current-level-departed-at
  [world]
  (update-current-level world #(level/set-departed-at % (current-time world))))

(defn- load-or-generate
  "Returns [world lvl]. Levels already visited stay in :levels, so generating
   and populating only happen on first arrival."
  [world id arrival-stair-kind]
  (if-let [lvl (level-at world id)]
    [world lvl]
    (let [lvl (dungeon/generate (:world-seed world) id)]
      (populate-level world lvl id #{(level/stair-pos lvl arrival-stair-kind)}))))

(defn- travel
  [world target-lvl-id arrival-stair-kind]
  (let [[world target-lvl] (load-or-generate world target-lvl-id arrival-stair-kind)]
    (-> world
        (set-current-level-departed-at)
        (change-current-level target-lvl-id target-lvl)
        (rebase-monster-clocks)
        (move-actor (player-id world) (level/stair-pos target-lvl arrival-stair-kind))
        (observe))))

(defn descend
  [world]
  (let [next-lvl-id (next-level-id world)
        new-world (travel world next-lvl-id :stairs/up)]
    [new-world [{:event/type :world/travelled
                 :direction  :down
                 :depth      (inc next-lvl-id)}]]))

(defn ascend
  [world]
  (let [prev-lvl-id (previous-level-id world)
        new-world (travel world prev-lvl-id :stairs/down)]
    [new-world [{:event/type :world/travelled
                 :direction  :up
                 :depth      (inc prev-lvl-id)}]]))

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
                     [world [{:event/type :world/invalid-action :action :stairs-down}]])

    :world/ascend (if (on-stairs-up? world)
                    (ascend world)
                    [world [{:event/type :world/invalid-action :action :stairs-up}]])
    :world/move (attempt-movement world actor-id (:delta action))
    :world/wait [world [{:event/type :world/wait}]]

    ;; default: throw an exception, because getting here is a mistake that shouldn't happen
    (throw (ex-info "unknown action type" {:action action}))))

;; the player is intentionally NOT in the per-level entity map, and is instead a global concept.
;; this is so I don't have to move the player in and out of the lists for the different levels.
;; the entire idea behind the "active-actors" function below is to facilitate this.
(defn new-world
  ([]
   (new-world 123456789))  ;; just some default seed
  ([seed]
   (let [first-dungeon (dungeon/generate seed 0)
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
                :rng-state (rng/make seed)}
         [world first-dungeon] (populate-level world first-dungeon 0 #{player-start-pos})]
     (-> world
         (put-level 0 first-dungeon)
         (observe)))))
