(ns roguelike.render
  (:require [roguelike.world :as world]))

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
  [tg ui msg-row]
  (let [mode (:mode ui)]
    (case (:screen mode)
      :prompt (.putString tg 0 0 (:message mode))
      (when-let [msg (:current-msg ui)]
        (.putString tg 0 msg-row msg)))))

(defn draw-level
  [tg world start-row]
  (doseq [tile (world/current-level->tile-list world)]
    (let [[x y] (:pos tile)
          glyph (str (:glyph tile))]
      (.putString tg x (+ y start-row) glyph))))

(defn draw-game
  [screen game]
  (let [world (:world game)
        ui    (:ui    game)
        [player-x player-y] (:pos (:player world))
        dimensions (screen-dimensions screen)
        layout (calculate-layout dimensions)
        tg (.newTextGraphics screen)]
    (.clear screen)
    (draw-level tg world (:play-start-row layout))
    (.putString tg
                player-x
                (+ (:play-start-row layout) player-y)
                "@")
    (draw-message tg ui (:msg-row layout))
    (.refresh screen)))
