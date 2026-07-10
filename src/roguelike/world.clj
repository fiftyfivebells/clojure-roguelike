(ns roguelike.world
  (:require [roguelike.level :as level]))

(defn new-world
  []
  {:player {:id 0 :type :player :pos [10 12]}
   :current-level (level/test-level)
   :levels        []
   :next-entity-id 1})

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

;; TODO: is this redundant now? check later
(defn clamp-position
  "Takes in a world and a pair of coordinates. Checks against the boundary of the world before returning the
  new coordinates. The return value is either the coordinates that were passed (if they are within the bounds
  of the world) or the edge of the world."
  [world [new-x new-y]]
  (let [[width height] (level/level-dimensions (:current-level world))]
    [(max 0 (min new-x (- width 1)))
     (max 0 (min new-y (- height 1)))]))

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
    (clamp-position world [new-x new-y])))

(defn move-player
  "Moves the player in the world to the provided coordinates. Returns a new world with the player
  glyph at the new coordinates."
  [world [x y]]
  (let [pos (clamp-position world [x y])
        player-id (:id (:player world))]
    (update-actor world player-id #(assoc % :pos pos))))

;; TODO: is this the same thing that "attempt-movement" is trying to do?
;;       one I'm at parity, investigate
;; (defmulti bump-action
;;   "Multi method for dispatching to different functions to determing the behavior depending on tile type."
;;   (fn [_world destination-tile _coords]
;;     (get-in destination-tile [:tile :type])))
;; 
;; (defmethod bump-action :floor
;;   [world _tile destination-coords]
;;   (world/move-player world destination-coords))
;; 
;; (defmethod bump-action :wall
;;   [world _tile _destination-coords]
;;   (let [msg "You bumped into a wall."]
;;     (ui/add-message world msg)))


(defn attempt-movement
  [world delta]
  (let [new-pos (get-proposed-coords world delta)
        tile (get-tile world new-pos)]
    (case (get-in tile [:tile :type])
      :floor [(move-player world new-pos) {:type :moved}]
      :wall [world {:type :bumped-wall}]

      ;; default case
      [world nil])))

(defn update-world
  [world action]
  (let [action-type (name (:type action))]
    (case action-type
      "move" (attempt-movement world [(:dx action) (:dy action)]))))

