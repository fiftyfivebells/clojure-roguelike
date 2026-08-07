(ns roguelike.render
  (:require [roguelike.world :as world]
            [roguelike.ui.appearance :as appearance])
  (:import [com.googlecode.lanterna TextColor$ANSI TextColor$RGB]))

;; TODO: will probably need to untangle this at some point. I've got two things
;; conflated: hue and visibility. Probably :bright/:dim/:dark need to be
;; modifiers of colors rather than in the color table themselves.
(def ^:private style->color
  {:bright TextColor$ANSI/WHITE
   :dim    TextColor$ANSI/BLACK_BRIGHT
   :dark   TextColor$ANSI/BLACK
   :cyan   TextColor$ANSI/CYAN
   :pink   (TextColor$RGB. 255 105 180)
   :green  TextColor$ANSI/GREEN})

(defn- clamp
  [point minimum maximum]
  (cond
    (< point minimum) minimum
    (> point maximum) maximum
    :else point))

(defn- camera-origin-1d
  [player viewport level]
  (if (<= level viewport)
    0
    (let [origin (- player (quot viewport 2))
          maximum (- level viewport)]
      (clamp origin 0 maximum))))

(defn- camera-origin
  [[px py] [vw vh] [lw lh]]
  (let [origin-x (camera-origin-1d px vw lw)
        origin-y (camera-origin-1d py vh lh)]
    [origin-x origin-y]))

(defn world->screen
  [world-pos camera-origin]
  (- world-pos camera-origin))

(defn- screen-dimensions
  [screen]
  (let [size (.getTerminalSize screen)]
    {:width (.getColumns size) :height (.getRows size)}))

(defn- calculate-layout
  [{:keys [width height]}]
  {:play-start-row 1
   :msg-row (dec height)
   :viewport [width (- height 2)]})

(defn draw-message
  [tg ui msg-row]
  (.setForegroundColor tg (style->color :bright))
  (let [mode (:mode ui)]
    (case (:screen mode)
      :prompt (.putString tg 0 0 (:message mode))
      (when-let [msg (:current-msg ui)]
        (.putString tg 0 msg-row msg)))))

(defn draw-level
  [tg world origin [vw vh] start-row]
  (let [view (into {} (map (juxt :pos identity)) (world/level-view world origin [vw vh]))]
    (doseq [sy (range vh)
            sx (range vw)]
      (let [tile (view (mapv + origin [sx sy]))
            glyph (appearance/tile-glyph (:tile tile))
            style (appearance/state-style (:state tile))]
        (.setForegroundColor tg (style->color style))
        (.putString tg sx (+ start-row sy) (str glyph))))))

(defn draw-actors
  [tg world origin [vw vh] start-row]
  (doseq [entity (world/visible-actors world)]
    (let [[sx sy] (mapv world->screen (:pos entity) origin)]
      (when (and (< -1 sx vw) (< -1 sy vh))
        (let [glyph (appearance/entity->glyph (world/entity-type entity))
              color (appearance/entity->color (world/entity-type entity))]
          (.setForegroundColor tg (style->color color))
          (.putString tg sx (+ start-row sy) (str glyph)))))))

(defn draw-game
  [screen game]
  (let [world (:world game)
        ui    (:ui    game)
        dimensions (screen-dimensions screen)
        level-size (world/current-level-dimensions world)
        layout (calculate-layout dimensions)
        tg (.newTextGraphics screen)
        origin (camera-origin (world/player-pos world) (:viewport layout) level-size)]
    (.clear screen)
    (draw-level tg world origin (:viewport layout) (:play-start-row layout))
    (draw-actors tg world origin (:viewport layout) (:play-start-row layout))
    (draw-message tg ui (:msg-row layout))
    (.refresh screen)))
