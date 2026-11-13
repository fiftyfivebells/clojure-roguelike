(ns roguelike.view
  (:require [roguelike.knowledge :as knowledge]
            [roguelike.level :as level]
            [roguelike.world :as world]))

(defn current-level-dimensions
  [world]
  (level/dimensions (world/current-level world)))

(defn tile-classifier
  ([world]
   (let [visible (world/visible-cells world)
         curr-lvl (world/current-level world)
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
   (map (tile-classifier world) (level/level->tile-list (world/current-level world))))
  ([world origin [vw-max vh-max]]
   (map (tile-classifier world)
        (level/level->tile-list
         (world/current-level world) origin [vw-max vh-max]))))

(defn visible-actors
  "Gets a list of active entities in the current level whose position is currently visible to the
  player (per FOV), so monsters outside the player's sight radius don't render."
  [world]
  (let [visible (world/visible-cells world)]
    (filter #(contains? visible (:pos %)) (world/active-actors world))))

(defn visible-items
  [world]
  (let [visible (world/visible-cells world)]
    (filter #(contains? visible (:item/pos %)) (world/current-level-items world))))

(defn status
  "Gives some helpful info about the current world. Used for UI display info."
  [world]
  ;; TODO: maybe add accessors for these? Maybe a player namespace?
  {:current-hp (get-in world [:player :hp/current])
   :max-hp     (get-in world [:player :hp/max])
   :level      (world/current-level-id world)})
