(ns roguelike.spawn
  (:require [roguelike.level :as level]
            [roguelike.monster :as monster]
            [roguelike.pathfinding :as pathfinding]
            [roguelike.placement :as placement]
            [roguelike.rng :as rng]))

;; What share of a level's rooms hold a group. This is a fraction rather than a
;; fixed count so density stays put as levels grow: room count scales with level
;; area.
(def ^:private min-occupied-rooms 1/4)
(def ^:private max-occupied-rooms 1/2)

(defn- draw-group-count
  "Groups to place, given how many rooms the level has. Scatter puts at most one
   group in a room, so this never exceeds room-count."
  [ctx room-count]
  (rng/draw-int ctx
                (max 1 (int (* min-occupied-rooms room-count)))
                (inc (int (* max-occupied-rooms room-count)))))

(defn- room-positions
  "Enumerates all of the potential positions inside a room. Returns them as a
   list of [x y] positions."
  [room]
  (let [[rx ry rw rh] (:bounds room)]
    (for [x (range rx (+ rx rw))
          y (range ry (+ ry rh))]
      [x y])))

(defn- open?
  "A tile a monster can be spawned onto: walkable, unoccupied, and not already
   claimed by an earlier group in this same pass."
  [lvl claimed pos]
  (and (level/walkable-at? lvl pos)
       (nil? (level/entity-at lvl pos))
       (not (contains? claimed pos))))

(defn- group-positions
  "The n tiles nearest the anchor that a group can occupy, anchor included.
   Returns fewer than n when the anchor is walled in."
  [lvl claimed anchor n]
  (->> (pathfinding/distance-map #(open? lvl claimed %) anchor n)
       (sort-by (fn [[pos d]] [d pos]))
       (map key)
       (filter #(open? lvl claimed %))
       (take n)))

(defn- place-group
  [ctx lvl claimed eligible anchor]
  (let [[ctx species] (rng/draw-any ctx eligible)
        [lo hi] (monster/group-size species)
        [ctx n] (rng/draw-int ctx lo (inc hi))]
    [ctx (mapv #(monster/instantiate species %)
               (group-positions lvl claimed anchor n))]))

(defn monsters-for-level
  "Returns [ctx monsters], where monsters is a vector of monster entity maps for
   lvl and ctx holds the spawn rng advanced past this population. Callers persist
   that state on the level so later waves continue the stream rather than
   re-deriving it and repeating this one.

   Placement is deterministic for a given world-seed and level-id. Reserved is a
   set of positions to leave clear, which is how the player's arrival tile is
   kept free. The entities carry no :entity/id or :next-time; those are added by
   world at the call site."
  [world-seed level-id lvl reserved]
  (let [ctx {:rng-state (rng/make (rng/mix world-seed :spawn level-id))}
        eligible (monster/species-at-depth level-id)]
    (if (empty? eligible)
      [ctx []]
      (let [rooms (level/get-rooms lvl)
            [ctx group-count] (draw-group-count ctx (count rooms))
            [ctx anchors] (placement/scatter ctx rooms group-count
                                             room-positions
                                             #(open? lvl reserved %))
            [ctx _ monsters]
            (reduce (fn [[ctx claimed acc] anchor]
                      (let [[ctx group] (place-group ctx lvl claimed eligible anchor)]
                        [ctx
                         (into claimed (map :pos) group)
                         (into acc group)]))
                    [ctx (set reserved) []]
                    anchors)]
        [ctx monsters]))))
