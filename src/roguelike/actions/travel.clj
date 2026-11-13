(ns roguelike.actions.travel
  (:require [roguelike.world :as world]))

(defn- descend
  [world _actor-id _action]
  (if-not (world/on-stairs-down? world)
    [world [{:event/type :world/invalid-action :action :stairs-down}]]
    (let [next-lvl-id (world/next-level-id world)
          new-world (world/travel world next-lvl-id :stairs/up)]
      [new-world [{:event/type :world/travelled
                   :direction  :down
                   :depth      (inc next-lvl-id)}]])))

(defn- ascend
  [world _actor-id _action]
  (if-not (world/on-stairs-up? world)
    [world [{:event/type :world/invalid-action :action :stairs-up}]]
    (let [prev-lvl-id (world/previous-level-id world)
          new-world (world/travel world prev-lvl-id :stairs/down)]
      [new-world [{:event/type :world/travelled
                   :direction  :up
                   :depth      (inc prev-lvl-id)}]])))

(def actions
  {:world/descend {:cost 10 :perform descend}
   :world/ascend  {:cost 10 :perform ascend}})
