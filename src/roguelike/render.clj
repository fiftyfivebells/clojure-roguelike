(ns roguelike.render
  (:require [roguelike.world :as world]))

(defn draw-message
  [tg world]
  (let [mode (:mode world)]
    (case (:screen mode)
      :prompt (.putString tg 0 0 (:message mode))
      (when-let [msg (:current-msg world)]
        (.putString tg 0 (:msg-row world) msg)))))

(defn draw-world
  [screen world]
  (let [player-coord (:player world)
        tg (.newTextGraphics screen)]
    (.clear screen)
    (.putString tg
                (:x player-coord)
                (+ (:play-start-row world) (:y player-coord))
                "@")
    (draw-message tg world)
    (.refresh screen)))
