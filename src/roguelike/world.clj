(ns roguelike.world
  (:require [clojure.set :as set]
            [roguelike.combat :as combat]
            [roguelike.fov :as fov]
            [roguelike.inventory :as inventory]
            [roguelike.item :as item]
            [roguelike.item.flavor :as flavor]
            [roguelike.item.spawn :as item-spawn]
            [roguelike.knowledge :as knowledge]
            [roguelike.level :as level]
            [roguelike.rng :as rng]
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

(defn current-level-items
  [world]
  (level/all-items (current-level world)))

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

(defn update-player-inventory
  [world items]
  (update-actor world
                (player-id world)
                (fn [p]
                  (update p :inventory inventory/add-items items))))

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

(defn- max-scan-radius
  "A radius big enough that a shadowcast from any point reaches every corner
   of the level, used to find line-of-sight into lit rooms regardless of
   torch radius."
  [level]
  (let [[w h] (level/dimensions level)]
    (long (Math/ceil (Math/sqrt (+ (* w w) (* h h)))))))

(defn visible-cells
  "Takes a world and sight-radius and returns a set of tiles visible from the
   actor: everything within torch radius, plus anything in line-of-sight that
   belongs to a currently-lit room (see level/lit-tiles)."
  [world actor-id radius]
  (let [level (current-level world)
        opaque? (partial level/opaque-at? level)
        actor-pos (:pos (get-actor world actor-id))
        torch (fov/visible-cells opaque? actor-pos radius)
        lit (level/lit-tiles level)]
    (if (empty? lit)
      torch
      (set/union torch
                 (set/intersection (fov/visible-cells opaque? actor-pos (max-scan-radius level))
                                    lit)))))

(defn observe
  [world]
  (let [world (update-current-level world #(level/light-room-at % (player-pos world)))
        radius (sight-radius world (player-id world))
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

(defn visible-items
  [world]
  (let [visible (visible-cells world (player-id world) (sight-radius world (player-id world)))]
    (filter #(contains? visible (:item/pos %)) (current-level-items world))))

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

(defn- learn-item-kind
  "Marks an item kind as identified. Knowledge is per kind, so every item of
  that kind is known from now on."
  [world item-kind]
  (update-actor world (player-id world)
                #(update-in % [:knowledge :item/known] conj item-kind)))

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

(defn- describe
  [world entity]
  (if (player? world (:entity/id entity))
    {:player? true}
    {:id (:entity/id entity) :species (:monster/species entity)}))

(defn- resolve-death
  [world defender]
  (if (:player? defender)
    (update-actor world (player-id world) #(assoc % :hp/current 0 :dead? true))
    (update-current-level world #(level/remove-entity % (:id defender)))))

(defn- apply-attack
  [world attack attacker defender]
  (let [base-result {:attacker (describe world attacker)
                     :defender (describe world defender)}]
    (if-not (:hit? attack)
      [world (merge base-result {:event/type :world/missed
                                 :hit? false
                                 :method (:method attack)})]
      (let [attack-result (merge attack base-result)]
        (if (:fatal? attack)
          [(resolve-death world (:defender base-result))
           (assoc attack-result :event/type :world/killed)]
          (let [new-hp (- (:hp/current defender) (:damage attack))
                world (update-actor world
                                    (:entity/id defender)
                                    #(assoc % :hp/current new-hp))]
            [world (assoc attack-result :event/type :world/hit)]))))))

(defn- apply-attacks
  [world attacks attacker defender]
  (let [reducer (fn
                  [[world events] attack]
                  (let [defender (get-actor world (:entity/id defender))
                        [world event] (apply-attack world attack attacker defender)]
                    [world (conj events event)]))]
    (reduce reducer [world []] attacks)))

(defn- attempt-attack
  [world actor-id new-pos]
  (let [attacker (get-actor world actor-id)
        defender (entity-at world new-pos)]
    (if (and (not (player? world actor-id))
             (not (player? world (:entity/id defender))))
      [world [{:event/type :world/blocked :by :actor :player? false}]]
      (let [atk-profile (combat/attacker-profile attacker)
            def-profile (combat/defender-profile defender)
            [world resolved] (combat/resolve-attacks world atk-profile def-profile) ]
        (apply-attacks world resolved attacker defender)))))

(defn- observed-items
  [world items]
  (let [known (->> items
                   (map :item/kind)
                   (filter #(known-item? world %)))]

    {:event/type :world/items-seen
     :items items
     :known (set known)}))

(defn attempt-movement
  [world actor-id delta]
  (let [new-pos (get-proposed-coords world actor-id delta)
        destination (classify-destination world new-pos)
        player? (player? world actor-id)]
    (cond
      ;; TODO: here, I think I need to add some kind of a flag to show whether
      ;; player is on items or not. If the player is moving onto items, then we
      ;; need to display a message (ie. "You see a <whatever> here."). Probably
      ;; something like :items? on the event so the ui knows whether to add a
      ;; message or proceed as normal
      (= destination :passable)
      (let [new-world (move-actor world actor-id new-pos)
            move-event {:event/type
                        :world/moved
                        :player? player?}
            items (level/items-at (current-level world) new-pos)]
        (if (and player? items)
          [new-world [move-event (observed-items world items)]]
          [new-world [move-event]]))
      
      (= destination :actor)
      (attempt-attack world actor-id new-pos)

      :else
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
   and populating only happen on first arrival. When generating, this function
   avoids spawning monsters within sight of the player's arrival location."
  [world id arrival-stair-kind]
  (if-let [lvl (level-at world id)]
    [world lvl]
    (let [lvl (dungeon/generate (:world-seed world) id)
          arrival-stairs (level/stair-pos lvl arrival-stair-kind)
          opaque? (partial level/opaque-at? lvl)
          seen (fov/visible-cells
                opaque?
                arrival-stairs
                (sight-radius world (player-id world)))]
      (populate-level world lvl id seen))))

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

(defn- heal-player
  [world]
  (update-actor world (player-id world)
                (fn [player]
                  (update player :hp/current #(min (:hp/max player) (inc %))))))

;; Item effects

(defmulti apply-effect
  "Applies one item effect to an actor. Effects are data in the item catalog,
  like {:effect/type :heal :dice 2 :sides 4}; this interprets them.
  Returns [world events]."
  (fn [_world _actor-id effect] (:effect/type effect)))

(defmethod apply-effect :heal
  [world actor-id effect]
  (let [[world amount] (rng/roll-dice world effect)
        world (update-actor world actor-id
                            (fn [actor]
                              (update actor :hp/current #(min (:hp/max actor) (+ % amount)))))]
    [world [{:event/type :world/healed :amount amount}]]))

(defmethod apply-effect :teleport
  ;; Rolls a distance from the effect's dice, then lands on a random open
  ;; square whose distance from the start is as close to that roll as the
  ;; level allows.
  [world actor-id effect]
  (let [start (:pos (get-actor world actor-id))
        [world distance] (rng/roll-dice world effect)
        off-by #(abs (- distance (Math/round (Math/sqrt (dungeon/distance-sq start %)))))
        ;; the player isn't in the level's entities, so we claim the start square
        spots (spawn/open-positions (current-level world) #{start})
        best (apply min (map off-by spots))
        [world dest] (rng/draw-any world (filterv #(= best (off-by %)) spots))]
    [(move-actor world actor-id dest)
     [{:event/type :world/teleported :from start :to dest}]]))

(defmethod apply-effect :damage
  [world actor-id effect]
  (let [[world amount] (rng/roll-dice world effect)
        actor (get-actor world actor-id)
        hp (- (:hp/current actor) amount)
        damaged {:event/type :world/damaged :amount amount}]
    (if (pos? hp)
      [(update-actor world actor-id #(assoc % :hp/current hp)) [damaged]]
      (let [victim (describe world actor)]
        [(resolve-death world victim)
         [damaged {:event/type :world/killed :defender victim}]]))))

(defn- apply-effects
  [world actor-id effects]
  (reduce (fn [[world events] effect]
            (let [[world new-events] (apply-effect world actor-id effect)]
              [world (into events new-events)]))
          [world []]
          effects))

(defn- pack-label
  "The letter the item with this id has in the player's pack, or nil."
  [world item-id]
  (some #(when (= item-id (:item/id (:item %))) (:label %)) (player-pack world)))

;; TODO: this function currently emits one event that carries only the first
;; item that was picked up. This is fine if the player picks up a single item,
;; or a stack of the same item, but it loses info if there are multiple items
;; picked up. Say the player picks up 2 potions and a dagger. This function only
;; carries the first item picked up, so we only send one message. I want to
;; refactor this eventually to emit messages for all the items, but this is part
;; of a larger refactor of the message log.
(defn- attempt-pickup
  [world actor-id]
  (let [current-pos (:pos (get-actor world actor-id))
        items (level/items-at (current-level world) current-pos)]
    (if (seq items)
      (let [new-world (-> world
                          (update-player-inventory items)
                          (update-current-level #(level/remove-items % current-pos)))
            new-item (inventory/find-item (player-inventory new-world) (first items))]
        [new-world [{:event/type :world/picked-up :item new-item :known (known-items world)}]])
      [world [{:event/type :world/pickup-failed :reason :no-item}]])))

(defn- drop-item
  [world actor-id item-id]
  (let [current-pos (:pos (get-actor world actor-id))
        item (inventory/find-item-by-id (player-inventory world) item-id)
        world (-> world
                  (update-actor (player-id world) #(update % :inventory inventory/remove-stack item-id))
                  (update-current-level #(level/add-item % current-pos (assoc item :item/pos current-pos))))]
    [world [{:event/type :world/dropped :item item}]]))

(defn- use-item
  "The player uses one item from the stack with the given id. The item is used
  up, its kind becomes known, and its effects are applied. The :world/<verb>
  event carries the item as it was before using, whether this use identified it,
  and the stack's letter (where it sits now, or where it was if none are left);
  effect events follow it."
  [world item-id event-type]
  (let [item (inventory/find-item-by-id (player-inventory world) item-id)]
    (when (nil? item)
      (throw (ex-info "no item with that id in the player's inventory" {:item/id item-id})))
    (let [kind (:item/kind item)
          learned? (not (known-item? world kind))
          label-before (pack-label world item-id)
          world (-> world
                    (update-actor (player-id world) #(update % :inventory inventory/remove-one item-id))
                    (learn-item-kind kind))
          label (or (pack-label world item-id) label-before)
          [world effect-events] (apply-effects world (player-id world) (:item/effects item))]
      [world (into [{:event/type event-type :item item :learned? learned? :label label}]
                   effect-events)])))

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

    :world/wait (let [world (if (player? world actor-id) (heal-player world) world)]
                  [world [{:event/type :world/wait}]])

    :world/get (attempt-pickup world actor-id)

    :world/drop (drop-item world actor-id (:item-id action))

    :world/quaff (use-item world (:item-id action) :world/quaffed)

    :world/read (use-item world (:item-id action) :world/read-scroll)

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
                         :next-time 0
                         :hp/current 15
                         :hp/max 15
                         :armor-class 0
                         :evasion 10
                         :to-hit 150
                         :attacks [{:method :smash :effect :physical :dice 3 :sides 4}]
                         :inventory []
                         :knowledge {:item/known #{}}}
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

(defn status
  "Gives some helpful info about the current world. Used for UI display info."
  [world]
  {:current-hp (get-in world [:player :hp/current])
   :max-hp     (get-in world [:player :hp/max])
   :level      (current-level-id world)})
