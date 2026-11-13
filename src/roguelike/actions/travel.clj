(ns roguelike.actions.travel
  (:require [roguelike.dungeon :as dungeon]
            [roguelike.fov :as fov]
            [roguelike.level :as level]
            [roguelike.world :as world]))

(defn- load-or-generate
  "Returns [world lvl]. Levels already visited stay in :levels, so generating
   and populating only happen on first arrival. When generating, this function
   avoids spawning monsters within sight of the player's arrival location."
  [world id arrival-stair-kind]
  (if-let [lvl (world/level-at world id)]
    [world lvl]
    (let [lvl (dungeon/generate (:world-seed world) id) ;; TODO: maybe add accessor for :world-seed
          arrival-stairs (level/stair-pos lvl arrival-stair-kind)
          opaque? (partial level/opaque-at? lvl)
          seen (fov/visible-cells
                opaque?
                arrival-stairs
                (world/sight-radius world (world/player-id world)))]
      (world/populate-level world lvl id seen))))

(defn travel
  [world target-lvl-id arrival-stair-kind]
  (let [[world target-lvl] (load-or-generate world target-lvl-id arrival-stair-kind)]
    (-> world
        (world/set-current-level-departed-at)
        (world/change-current-level target-lvl-id target-lvl)
        (world/rebase-monster-clocks)
        (world/move-actor (world/player-id world) (level/stair-pos target-lvl arrival-stair-kind))
        (world/observe))))

(defn next-level-id
  [world]
  (inc (world/current-level-id world)))

(defn previous-level-id
  [world]
  (dec (world/current-level-id world)))

(defn- descend
  [world _actor-id _action]
  (if-not (world/on-stairs-down? world)
    [world [{:event/type :world/invalid-action :action :stairs-down}]]
    (let [next-lvl-id (next-level-id world)
          new-world (travel world next-lvl-id :stairs/up)]
      [new-world [{:event/type :world/travelled
                   :direction  :down
                   :depth      (inc next-lvl-id)}]])))

(defn- ascend
  [world _actor-id _action]
  (if-not (world/on-stairs-up? world)
    [world [{:event/type :world/invalid-action :action :stairs-up}]]
    (let [prev-lvl-id (previous-level-id world)
          new-world (travel world prev-lvl-id :stairs/down)]
      [new-world [{:event/type :world/travelled
                   :direction  :up
                   :depth      (inc prev-lvl-id)}]])))



(def actions
  {:world/descend {:cost 10 :perform descend}
   :world/ascend  {:cost 10 :perform ascend}})
