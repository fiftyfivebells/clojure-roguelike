(ns roguelike.game
  (:require [roguelike.scheduler :as scheduler]
            [roguelike.world :as world]
            [roguelike.ui    :as ui]))

(defn new-game
  []
  {:world (world/new-world)
   :ui    (ui/new-ui)})

(defn advance
  [game]
  (let [[new-world _ status] (scheduler/advance (:world game))]
    [(assoc game :world new-world) status]))

(defn update-game
  [game action]
  (case (namespace (:action/type action))
    "world"
    (let [[new-world events] (scheduler/resolve-action (:world game) 0 action)
          new-ui             (ui/apply-events (:ui game) events)]
      (-> game
          (assoc :world new-world)
          (assoc :ui new-ui)))

    "ui"
    (update game :ui ui/update-mode action)

    (throw (ex-info "unroutable action" {:action action}))))
