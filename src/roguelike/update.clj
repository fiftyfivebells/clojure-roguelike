(ns roguelike.update
  (:require [roguelike.world :as world]))

(defn update-world
  [world input]
  (case (:type input)
    :move (world/move-player world {:x (:dx input) :y (:dy input)})
    :quit (assoc world :mode :quit)
    :none world))
