(ns roguelike.item.spawn
  (:require [roguelike.rng :as rng]
            [roguelike.item :as item]
            [roguelike.level :as level]))

;; ~5% chance (1/20) to "boost" the spawned item. It allows items that are out
;; of depth for the current level. Taken from Angband.
(def ^:private boost-chance 20)

;; Tunable min/max values for the number of items that can spawn per level.
;; Right now these are basically random, but they should probably eventually
;; be derived from level size.
(def ^:private min-items 40)
(def ^:private max-items 150)

;; TODO: this calculation can/should be tuned
(defn- derive-item-budget
  "Determines the number of items to spawn for the given level. Currently uses
  the min and max items constants to get a minimum number, and then adds the
  current level id divided by 5 to the value. The deeper the player is, the more
  items spawn per level."
  [ctx level-id]
  (let [[ctx total-items] (rng/draw-int ctx min-items (inc max-items))]
    [ctx (+ total-items (quot level-id 5))]))

(defn- boost-depth
  "Returns a depth value for 'boosting' the current item. This lets out of depth
  items spawn before they normally would. The first roll determines whether the
  depth should be boosted or not. The second roll determines the divisor. We
  multiply the current depth by the max depth, then divide it by the divisor we
  rolled. The boosted depth is the min of max depth and the calculated value."
  [ctx depth]
  (let [[ctx boost-roll] (rng/draw-int ctx 0 boost-chance)
        boost? (and (pos? depth) (zero? boost-roll))]
    (if boost?
      (let [[ctx divisor] (rng/draw-int ctx 1 (inc item/max-depth))
            depth (min item/max-depth (inc (quot (* depth item/max-depth) divisor)))]
        [ctx depth])
      [ctx depth])))

(defn- draw-item
  "Draws one item from the available items, weighted by the commonness value. The
  boost is rolled per item."
  [ctx depth]
  (let [[ctx boosted] (boost-depth ctx depth)
        eligible (item/items-at-depth boosted)
        items (if (seq eligible) eligible (item/items-at-depth depth))]
    (rng/draw-weighted ctx items item/commonness)))

(defn- open-positions
  "Walkable, unreserved tiles. In contrast with the open positions for monsters,
  items can spawn even on spots occupied by monsters."
  [lvl reserved]
  (let [[w h] (level/dimensions lvl)]
    (for [x (range w)
          y (range h)
          :let [pos [x y]]
          :when (and (level/walkable-at? lvl pos)
                     (not (contains? reserved pos)))]
      pos)))

(defn items-for-level
  [world-seed level-id lvl reserved]
  (let [ctx {:rng-state (rng/mix-and-make world-seed :items level-id)}]
    (if (empty? (item/items-at-depth level-id))
      [ctx []]
      (let [[ctx positions] (rng/shuffle-coll ctx (open-positions lvl (set reserved)))
            [ctx budget] (derive-item-budget ctx level-id)]
        (loop [ctx ctx
               acc []
               budget (min budget (count positions))
               cursor 0]
          (if (zero? budget)
            [ctx acc]
            (let [[ctx item] (draw-item ctx level-id)]
              (recur ctx
                     (conj acc [(nth positions cursor) (item/instantiate item)])
                     (dec budget)
                     (inc cursor)))))))))
