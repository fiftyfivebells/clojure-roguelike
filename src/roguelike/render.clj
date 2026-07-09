(ns roguelike.render
  (:require [roguelike.level :as level]))

(defn- screen-dimensions
  [screen]
  (let [size (.getTerminalSize screen)]
    {:width (.getColumns size) :height (.getRows size)}))

(defn- calculate-layout
  [dimensions]
  (let [width  (:width dimensions)
        height (:height dimensions)]
    {:play-start-row 1
     :msg-row (- height 1)
     :map-rows (- height 2)}))

(defn draw-message
  [tg world msg-row]
  (let [mode (:mode (:ui world))
        ui   (:ui world)]
    (case (:screen mode)
      :prompt (.putString tg 0 0 (:message mode))
      (when-let [msg (:current-msg ui)]
        (.putString tg 0 msg-row msg)))))

(defn draw-level
  [tg level start-row]
  (doseq [tile (level/level->tile-list level)]
    (let [x (:x tile)
          y (:y tile)
          glyph (str (:glyph tile))]
      (.putString tg x (+ y start-row) glyph))))

(defn draw-world
  [screen world]
  (let [game (:game world)
        [player-x player-y] (:pos (:player game))
        dimensions (screen-dimensions screen)
        layout (calculate-layout dimensions)
        tg (.newTextGraphics screen)]
    (.clear screen)
    (draw-level tg (:current-level game) (:play-start-row layout))
    (.putString tg
                player-x
                (+ (:play-start-row layout) player-y)
                "@")
    (draw-message tg world (:msg-row layout))
    (.refresh screen)))
