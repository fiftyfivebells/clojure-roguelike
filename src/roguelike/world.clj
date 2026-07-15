(ns roguelike.world
  (:require [roguelike.level :as level]
            [roguelike.rng :as rng]))

(defn- cost-of
  [action]
  (case (:type action)
    :world/move 10
    :world/wait 10
    (throw (ex-info "this action doesn't exist" {:action action}))))

;; the player is intentionally NOT in the per-level entity map, and is instead a global concept.
;; this is so I don't have to move the player in and out of the lists for the different levels.
;; the entire idea behind the "active-actors" function below is to facilitate this.
(defn new-world
  ([]
   (new-world 123456789))  ;; just some default seed
  ([seed]
   {:player {:id 0 :type :player :pos [10 12] :next-time 0}
    :current-level (level/test-level)
    :levels        []
    :next-entity-id 1
    :next-tick 10
    :current-time 0
    :rng-state (rng/make seed)}))

(defn current-level->tile-list
  "Takes a world and gives back a list of every tile in the current level along with its [x y] position."
  [world]
  (level/level->tile-list (:current-level world)))

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
  or :blocked-door. This is the seam that owns tile-based movement classification, so
  callers never need to read a tile's raw :type themselves."
  [world [x y]]
  (case (level/classify-tile (level/tile-at (:current-level world) [x y]))
    :floor       :passable
    :open-door   :passable
    :wall        :blocked-wall
    :closed-door :blocked-door
    :blocked-unknown))

(defn attempt-movement
  [world actor-id delta]
  (let [new-pos (get-proposed-coords world actor-id delta)
        destination (classify-destination world new-pos)]
    (if (= destination :passable)
      [(move-actor world actor-id new-pos) [{:type :moved}]]
      [world [{:type :blocked :by destination}]])))

(defn- update-world
  [world actor-id action]
  (case (:type action)
    :world/move (attempt-movement world actor-id [(:dx action) (:dy action)])
    :world/wait [world []]

    ;; default: just return world unchanged
    (throw (ex-info "unknown action type" {:action action}))))

(defn- reschedule
  [world actor-id cost]
  (update-actor world actor-id #(assoc % :next-time (+ (:current-time world) cost))))

(defn resolve-action
  [world actor-id action]
  (let [[new-world events] (update-world world actor-id action)
        rescheduled-world (reschedule new-world actor-id (cost-of action))]
    [rescheduled-world events]))

(defn next-scheduled
  "Looks at a world and returns what is next up in the event scheduler. It could be the player, an entity,
  or simply advancing the game time one 'tick'. Returns a map with the info for whichever has the lowest next-time.
  The priority for breaking ties in lowest time is:
    - actors are always before tick
    - between actors, lowest id wins always
  This gives a priority (everything else equal) of player -> monster -> tick."
  [world]
  (let [make-scheduled-actor (fn [{:keys [id next-time]}] {:kind :actor :id id :at next-time})
        priority (fn [{:keys [kind at id]}]
                   [at
                    (if (= kind :actor) 0 1)
                    (or id 0)])
        reducer (fn [best current]
                  (if (neg? (compare (priority current) (priority best)))
                    current
                    best))
        actors (map make-scheduled-actor (active-actors world))
        with-tick (conj actors {:kind :tick :at (:next-tick world)})]
    (reduce reducer with-tick)))

(defn- tick
  [world]
  (assoc world :next-tick (+ (:current-time world) 10)))

(defn advance
  "Takes a world and advances it. This involves checking through the actors list for the next entity to act,
  then resolving whatever action that actor needs to take. Right now, this is:
  - player: returns an :awaiting-input status (needs player input at this point)
  - monster: uses monster ai to decide on some action for the monster to take
  - tick: 'ticks' the world forward to the next time unit"
  [world]
  (let [scheduled (next-scheduled world)
        next-world (assoc world :current-time (:at scheduled))]
    (cond 
      (= (:kind scheduled) :tick)
      (let [ticked-world (tick next-world)]
          [ticked-world [] :ticked])

      (player? world (:id scheduled)) [next-world [] :awaiting-input]

      ;; else branch: decide the entity's next action and resolve it
      :else (resolve-action next-world (:id scheduled) {:type :world/wait}))))
