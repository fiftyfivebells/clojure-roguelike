(ns roguelike.effects
  (:require [roguelike.dungeon :as dungeon]
            [roguelike.rng :as rng]
            [roguelike.spawn :as spawn]
            [roguelike.world :as world]))

(defmulti apply-effect
  "Applies one item effect to an actor. Effects are data in the item catalog,
  like {:effect/type :heal :dice 2 :sides 4}; this interprets them.
  Returns [world events]."
  (fn [_world _actor-id effect] (:effect/type effect)))

(defmethod apply-effect :heal
  [world actor-id effect]
  (let [[world amount] (rng/roll-dice world effect)
        world (world/update-actor world actor-id
                            (fn [actor]
                              (update actor :hp/current #(min (:hp/max actor) (+ % amount)))))]
    [world [{:event/type :world/healed :amount amount}]]))

(defmethod apply-effect :teleport
  ;; Rolls a distance from the effect's dice, then lands on a random open
  ;; square whose distance from the start is as close to that roll as the
  ;; level allows.
  [world actor-id effect]
  (let [start (:pos (world/get-actor world actor-id))
        [world distance] (rng/roll-dice world effect)
        off-by #(abs (- distance (Math/round (Math/sqrt (dungeon/distance-sq start %)))))
        ;; the player isn't in the level's entities, so we claim the start square
        spots (spawn/open-positions (world/current-level world) #{start})
        best (apply min (map off-by spots))
        [world dest] (rng/draw-any world (filterv #(= best (off-by %)) spots))]
    [(world/move-actor world actor-id dest)
     [{:event/type :world/teleported :from start :to dest}]]))

(defmethod apply-effect :damage
  [world actor-id effect]
  (let [[world amount] (rng/roll-dice world effect)
        actor (world/get-actor world actor-id)
        hp (- (:hp/current actor) amount)
        damaged {:event/type :world/damaged :amount amount}]
    (if (pos? hp)
      [(world/update-actor world actor-id #(assoc % :hp/current hp)) [damaged]]
      (let [victim (world/describe world actor)]
        [(world/resolve-death world victim)
         [damaged {:event/type :world/killed :defender victim}]]))))

(defn apply-effects
  [world actor-id effects]
  (reduce (fn [[world events] effect]
            (let [[world new-events] (apply-effect world actor-id effect)]
              [world (into events new-events)]))
          [world []]
          effects))
