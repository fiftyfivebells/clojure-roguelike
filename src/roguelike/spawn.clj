(ns roguelike.spawn
  (:require [roguelike.level :as level]
            [roguelike.monster :as monster]
            [roguelike.pathfinding :as pathfinding]
            [roguelike.rng :as rng]))

;; What share of a level's rooms hold a group. This is a fraction rather than a
;; fixed count so density stays put as levels grow: room count scales with level
;; area.
(def ^:private min-occupied-rooms 0.25)
(def ^:private max-occupied-rooms 0.50)

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

(defn open-positions
  [lvl claimed]
  (let [[w h] (level/dimensions lvl)]
    (for [x (range w)
          y (range h)
          :let [pos [x y]]
          :when (open? lvl claimed pos)]
      pos)))

(defn- place-group
  [ctx lvl claimed eligible anchor]
  (let [[ctx species] (rng/draw-any ctx eligible)
        [lo hi] (monster/group-size species)
        [ctx n] (rng/draw-int ctx lo (inc hi))
        make-monster (fn [[ctx acc] pos]
                        (let [[ctx monster] (monster/instantiate ctx species pos)]
                          [ctx (conj acc monster)]))]
    (reduce make-monster
            [ctx []]
            (group-positions lvl claimed anchor n))))

(defn derive-monster-budget
  [ctx lvl-id open]
  (let [monster-min (int (/ (count open) 200))
        [ctx rand-int] (rng/draw-int ctx 2 8)
        depth (int (/ lvl-id 3))
        depth-cnt (max 2 (min depth 10))]
    [ctx (+ monster-min rand-int depth-cnt)]))

(defn monsters-for-level
  [world-seed level-id lvl reserved]
  (let [ctx {:rng-state (rng/mix-and-make world-seed :spawn level-id)}
        eligible (monster/species-at-depth level-id)]
    (if (empty? eligible)
      [ctx []]
      (let [[ctx shuffled] (rng/shuffle-coll ctx (open-positions lvl reserved))
            [ctx monster-budget] (derive-monster-budget ctx level-id shuffled)]
        (loop [ctx ctx
               claimed (set reserved)
               acc []
               monster-budget monster-budget
               cursor 0]
          (cond
            (<= monster-budget 0)
            [ctx acc]

            (>= cursor (count shuffled))
            [ctx acc]

            (claimed (shuffled cursor))
            (recur ctx claimed acc monster-budget (inc cursor))

            :else
            (let [anchor (shuffled cursor)
                  [ctx group] (place-group ctx lvl claimed eligible anchor)]
              (recur ctx
                     (into claimed (map :pos) group)
                     (into acc group)
                     (- monster-budget (count group))
                     (inc cursor)))))))))
