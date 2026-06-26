(ns roguelike.render
  (:require [lanterna.screen :as s]
            [roguelike.world :as world]))

(defn draw-world
  [screen world]
  (let [player-coord (:player world)]
    (s/clear screen)
    (s/put-string screen (:x player-coord) (:y player-coord) "@")
    (s/redraw screen)))
