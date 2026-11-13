(ns roguelike.world
  (:require [roguelike.inventory :as inventory]
            [roguelike.item :as item]
            [roguelike.item.flavor :as flavor]
            [roguelike.item.spawn :as item-spawn]
            [roguelike.knowledge :as knowledge]
            [roguelike.level :as level]
            [roguelike.player :as player]
            [roguelike.rng :as rng]
            [roguelike.spawn :as spawn]
            [roguelike.dungeon :as dungeon]
            [roguelike.vision :as vision]))

;; Levels

(defn level-at
  "Takes a world and id and returns the level associated with that id. This is
   intentionally allowed to return nil, so that callers can use the nil as a
   signal that a new level should be created using the provided id."
  [world id]
  (get-in world [:levels id]))

(defn- put-level
  [world id lvl]
  (assoc-in world [:levels id] lvl))

(defn current-level-id
  [world]
  (:current-level-id world))

(defn- set-current-level-id
  [world id]
  (assoc world :current-level-id id))

(defn current-level
  "Returns the currently active level of the world."
  [world]
  (level-at world (current-level-id world)))

(defn update-current-level
  [world f]
  (let [id (current-level-id world)]
    (put-level world id (f (level-at world id)))))

(defn current-level-items
  [world]
  (level/all-items (current-level world)))

(defn items-at
  [world pos]
  (level/items-at (current-level world) pos))

(defn change-current-level
  [world id lvl]
  (-> world
      (set-current-level-id id)
      (put-level id lvl)))

;; Time

(defn- current-time
  [world]
  (:current-time world))

(defn rebase-monster-clocks
  [world]
  (update-current-level
   world
   (fn [lvl]
     (let [departed (or (level/departed-at lvl) (current-time world))]
       (level/rebase-entity-time lvl (- (current-time world) departed))))))

;; World Construction

(defn allocate-entity-id
  [world]
  (let [next-entity-id (:next-entity-id world)]
    ;; this when branch should not be possible to reach, it's a "just in case" safety net
    (when (= 0 next-entity-id)
      (throw (ex-info "entity id 0 is reserved for the player" {:entity-id next-entity-id})))

    [next-entity-id (update world :next-entity-id inc)]))

(defn populate-level
  "Spawns this level's monsters into lvl, giving each one an id from the world's
   counter and a clock set to now. Reserved positions are left empty so the
   player never lands on top of a monster. The spawn rng is left on the level so
   later waves of monsters pick up where this one stopped. Returns [world lvl]."
  [world lvl lvl-id reserved]
  (let [[_ pairs] (item-spawn/items-for-level (:world-seed world) lvl-id lvl reserved)
        [world lvl] (reduce (fn [[world lvl] [pos item]]
                              (let [[id world] (allocate-entity-id world)]
                                [world (level/add-item lvl pos (assoc item :item/id id :item/pos pos))]))
                            [world lvl]
                            pairs)
        [ctx monsters] (spawn/monsters-for-level (:world-seed world) lvl-id lvl reserved)
        [world lvl] (reduce (fn [[world lvl] monster]
                              (let [[id world] (allocate-entity-id world)]
                                [world (level/add-entity lvl (assoc monster
                                                                    :entity/id id
                                                                    :next-time (current-time world)))]))
                            [world lvl]
                            monsters)]
    [world (level/set-rng-state lvl (:rng-state ctx))]))

;; Actors

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

(defn player-dead?
  [world]
  (let [player (player-entity world)]
    (<= (:hp/current player) 0)))

(defn player-knowledge
  [world]
  (:knowledge (player-entity world)))

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

(defn player-inventory
  [world]
  (:inventory (player-entity world)))

(defn player-equipment
  [world]
  (:equipment (player-entity world)))

(defn update-player-inventory
  [world items]
  (update-actor world
                (player-id world)
                (fn [p]
                  (update p :inventory inventory/add-items items))))

(defn set-current-level-departed-at
  [world]
  (update-current-level world #(level/set-departed-at % (current-time world))))

(defn on-stairs-down?
  [world]
  (level/on-stairs-down? (current-level world) (player-pos world)))

(defn on-stairs-up?
  [world]
  (level/on-stairs-up? (current-level world) (player-pos world)))

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
  [world]
  (get-in world [:vision :visible]))

(defn observe
  [world]
  (let [world (update-current-level world #(level/light-room-at % (player-pos world)))
        radius (sight-radius world (player-id world))
        v (vision/compute (current-level world) (player-pos world) radius)]
    (-> world
        (assoc :vision v)
        (update-current-level #(level/remember-visible % (:visible v))))))


;; Knowledge

(defn flavors
  "Returns the flavors rolled for this world (e.g., which potion is 'bubbling')."
  [world]
  (:flavors world))

(defn known-items
  "The set of item kinds the player has identified."
  [world]
  (get-in (player-entity world) [:knowledge :item/known]))

(defn player-pack
  "The player's inventory in display order, with letters (see inventory/pack)."
  [world]
  (inventory/pack (player-inventory world) (flavors world) (known-items world)))

(defn known-item?
  [world item-kind]
  (let [player (player-entity world)]
    (contains? (get-in player [:knowledge :item/known]) item-kind)))

(defn learn-item-kind
  "Marks an item kind as identified. Knowledge is per kind, so every item of
  that kind is known from now on."
  [world item-kind]
  (update-actor world (player-id world)
                #(update-in % [:knowledge :item/known] conj item-kind)))

;; Movement

(defn walkable-at?
  [world [x y]]
  (level/walkable-at? (current-level world) [x y]))

(defn move-actor
  "Moves the actor in the world to the provided coordinates. Returns a new world with the actor
  glyph at the new coordinates."
  [world actor-id [x y]]
  (update-actor world actor-id #(assoc % :pos [x y])))

;; Effects

(defn describe
  [world entity]
  (if (player? world (:entity/id entity))
    {:player? true}
    {:id (:entity/id entity) :species (:monster/species entity)}))

(defn resolve-death
  [world defender]
  (if (:player? defender)
    (update-actor world (player-id world) #(assoc % :hp/current 0 :dead? true))
    (update-current-level world #(level/remove-entity % (:id defender)))))


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
                :player (player/new-player player-start-pos)
                :current-level-id 0
                :levels {0 first-dungeon}
                :next-entity-id 1
                :next-tick 10
                :current-time 0
                :rng-state (rng/make seed)
                :flavors (flavor/roll-flavors seed item/flavored-items)}
         [world first-dungeon] (populate-level world first-dungeon 0 #{player-start-pos})]
     (-> world
         (put-level 0 first-dungeon)
         (observe)))))


