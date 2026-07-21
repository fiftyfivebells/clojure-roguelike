(ns roguelike.render
  (:require [roguelike.world :as world])
  (:import [com.googlecode.lanterna TextColor$ANSI TextColor$RGB]))

(def ^:private state->style
  {:visible    :bright
   :remembered :dim
   :unknown    :dark})

(def ^:private style->color
  {:bright TextColor$ANSI/WHITE
   :dim    TextColor$ANSI/BLACK_BRIGHT
   :dark   TextColor$ANSI/BLACK
   :pink   (TextColor$RGB. 255 105 180)
   :green  TextColor$ANSI/GREEN})

(def ^:private tile->glyph
  {:wall    "#"
   :floor   "."
   :door    "+"
   :unknown " "})

(def ^:private entity->glyph
  {:player          "@"
   :generic-monster "m"})

(def ^:private entity->style
  {:player          :pink
   :generic-monster :green})

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
  (.setForegroundColor tg (style->color :bright))
  (let [mode (:mode ui)]
    (case (:screen mode)
      :prompt (.putString tg 0 0 (:message mode))
      (when-let [msg (:current-msg ui)]
        (.putString tg 0 msg-row msg)))))

(defn draw-level
  [tg world start-row]
  (doseq [tile (world/level-view world)]
    (let [[x y] (:pos tile)
          glyph (tile->glyph (:tile tile))
          style (state->style (:state tile))]
      (.setForegroundColor tg (style->color style))
      (.putString tg x (+ y start-row) glyph))))

(defn draw-actors
  [tg world start-row]
  (doseq [entity (world/visible-actors world)]
    (let [[x y] (:pos entity)
          glyph (entity->glyph (world/entity-type entity))
          style (entity->style (world/entity-type entity))]
      (.setForegroundColor tg (style->color style))
      (.putString tg x (+ start-row y) (str glyph)))))

(defn draw-game
  [screen game]
  (let [world (:world game)
        ui    (:ui    game)
        dimensions (screen-dimensions screen)
        layout (calculate-layout dimensions)
        tg (.newTextGraphics screen)]
    (.clear screen)
    (draw-level tg world (:play-start-row layout))
    (draw-actors tg world (:play-start-row layout))
    (draw-message tg ui (:msg-row layout))
    (.refresh screen)))
