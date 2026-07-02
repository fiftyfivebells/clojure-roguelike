(ns roguelike.render
  (:require [roguelike.level :as level]))

(defn draw-message
  [tg world]
  (let [mode (:mode world)]
    (case (:screen mode)
      :prompt (.putString tg 0 0 (:message mode))
      (when-let [msg (:current-msg world)]
        (.putString tg 0 (:msg-row world) msg)))))

(defn draw-level
  [tg level start-row]
  (doseq [tile (level/level->tile-list level)]
    (let [x (:x tile)
          y (:y tile)
          glyph (str (:glyph tile))]
      (.putString tg x (+ y start-row) glyph))))

(defn draw-world
  [screen world]
  (let [player-coord (:player world)
        tg (.newTextGraphics screen)]
    (.clear screen)
    (draw-level tg (:current-level world) (:play-start-row world))
    (.putString tg
                (:x player-coord)
                (+ (:play-start-row world) (:y player-coord))
                "@")
    (draw-message tg world)
    (.refresh screen)))
