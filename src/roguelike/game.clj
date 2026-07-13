(ns roguelike.game
  (:require [roguelike.world :as world]
            [roguelike.ui    :as ui]))

(defn new-game
  []
  {:world (world/new-world)
   :ui    (ui/new-ui)})

(defn update-game
  [game action]
  (case (namespace (:type action))
    "world"
    (let [[new-world event] (world/update-world (:world game) action)]
      (-> game
          (assoc :world new-world)
          (update :ui ui/apply-event event)))

    "ui"
    (update game :ui ui/update-mode action)

    (ex-info "unroutable action" {:action action})))

