(ns roguelike.render
  (:require [roguelike.world :as world]
            [roguelike.ui.equipment-select :as equipment-select]
            [roguelike.ui.item-select :as item-select]
            [roguelike.ui.number-select :as number-select]
            [roguelike.ui.appearance :as appearance]
            [roguelike.ui.appearance.item :as item]
            [roguelike.ui :as ui])
  (:import [com.googlecode.lanterna TextColor$ANSI TextColor$RGB]))

;; TODO: will probably need to untangle this at some point. I've got two things
;; conflated: hue and visibility. Probably :bright/:dim/:dark need to be
;; modifiers of colors rather than in the color table themselves.
(def ^:private style->color-map
  {:bright TextColor$ANSI/WHITE
   :dim    TextColor$ANSI/BLACK_BRIGHT
   :dark   TextColor$ANSI/BLACK
   :white  TextColor$ANSI/WHITE
   :cyan   TextColor$ANSI/CYAN
   :blue   TextColor$ANSI/BLUE
   :red    TextColor$ANSI/RED
   :purple (TextColor$RGB. 105 14 100)
   :pink   (TextColor$RGB. 255 105 180)
   :green  TextColor$ANSI/GREEN
   :brown  (TextColor$RGB. 139 69 19)})

(def ^:private default-color
  (TextColor$RGB. 255 0 255))

(defn- style->color
  [style]
  (get style->color-map style default-color))

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
   :play-start-col 12
   :msg-row (dec height)
   :viewport [(- width 12) (- height 2)]})

(defn draw-message
  [tg ui start-col msg-row]
  (.setForegroundColor tg (style->color :bright))
  (let [mode (ui/current-mode ui)]
    (case (:screen mode)
      :prompt (.putString tg start-col 0 (:message mode))
      :item-select nil
      :equipment-select nil
      :choice (.putString tg start-col 0 (:prompt mode))
      :number-select (.putString tg start-col 0 (number-select/prompt-line mode))
      (when-let [msg (:current-msg ui)]
        (.putString tg start-col msg-row msg)))))

(defn draw-level
  [tg world origin [vw vh] start-row start-col]
  (let [view (into {} (map (juxt :pos identity)) (world/level-view world origin [vw vh]))]
    (doseq [sy (range vh)
            sx (range vw)]
      (let [tile (view (mapv + origin [sx sy]))
            glyph (appearance/tile-glyph (:tile tile))
            style (appearance/state-style (:state tile))]
        (.setForegroundColor tg (style->color style))
        (.putString tg (+ sx start-col) (+ start-row sy) (str glyph))))))

(defn draw-items
  [tg world origin [vw vh] start-row start-col]
  (doseq [item (world/visible-items world)]
    (let [[sx sy] (mapv world->screen (:item/pos item) origin)]
      (when (and (< -1 sx vw) (< -1 sy vh))
        (let [glyph (item/glyph-of (:item/kind item))
              color (item/color-of item (world/flavors world))]
          (.setForegroundColor tg (style->color color))
          (.putString tg (+ sx start-col) (+ start-row sy) (str glyph)))))))

(defn draw-actors
  [tg world origin [vw vh] start-row start-col]
  (doseq [entity (world/visible-actors world)]
    (let [[sx sy] (mapv world->screen (:pos entity) origin)]
      (when (and (< -1 sx vw) (< -1 sy vh))
        (let [glyph (appearance/entity->glyph entity)
              color (appearance/entity->color entity)]
          (.setForegroundColor tg (style->color color))
          (.putString tg (+ sx start-col) (+ start-row sy) (str glyph)))))))

(defn- draw-stat-line
  [tg row width label value]
  (let [label-str (str label)
        value-str (str value)
        padding (max 1 (- width (count label-str) (count value-str)))
        line (str label-str (apply str (repeat padding \space)) value-str)]
    (.putString tg 0 row line)))

(defn draw-sidebar
  [tg world layout]
  (let [start-row (:play-start-row layout)
        width (dec (:play-start-col layout))
        {:keys [current-hp max-hp level]} (world/status world)]
    (.setForegroundColor tg (style->color :bright))
    (draw-stat-line tg start-row width "HP" (str current-hp "/" max-hp))
    (draw-stat-line tg (inc start-row) width "Depth" (inc level))))

(defn- draw-list
  "Draws a picker list over the map, similar to Angband. The header on the top
  line, then one row per entry."
  [tg prompt header rows width]
  (let [max-len (apply max (map #(+ (count (:label %)) (count (:text %))) rows))
        col (max 0 (min (- width 1 max-len) (- (max (count prompt) 15) 2)))
        blank (apply str (repeat (- width (dec col)) \space))]
    (.setForegroundColor tg (style->color :bright))
    (.putString tg 0 0 header)
    (doseq [[i {:keys [label text color]}] (map-indexed vector rows)]
      (let [row (inc i)]
        (.putString tg (dec col) row blank)
        (.setForegroundColor tg (style->color :bright))
        (.putString tg col row label)
        (.setForegroundColor tg (style->color color))
        (.putString tg (+ col (count label)) row text)))))

(defn draw-item-select
  [tg world frame width]
  (let [entries (item-select/selectable (world/player-pack world) (:filter frame))
        rows (item-select/rows entries (world/flavors world) (world/known-items world))]
    (draw-list tg (:prompt frame) (item-select/header (:prompt frame) entries) rows width)))

(defn draw-equipment-select
  [tg world frame width]
  (let [entries (equipment-select/entries (world/player-equipment world))
        rows (equipment-select/rows entries (world/flavors world) (world/known-items world))]
    (draw-list tg (:prompt frame) (equipment-select/header (:prompt frame) entries) rows width)))

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
    (draw-level tg world origin (:viewport layout) (:play-start-row layout) (:play-start-col layout))
    (draw-items tg world origin (:viewport layout) (:play-start-row layout) (:play-start-col layout))
    (draw-actors tg world origin (:viewport layout) (:play-start-row layout) (:play-start-col layout))
    (draw-message tg ui 12 (:msg-row layout))
    (draw-sidebar tg world layout)
    (let [mode (ui/current-mode ui)]
      (case (:screen mode)
        :item-select (draw-item-select tg world mode (:width dimensions))
        :equipment-select (draw-equipment-select tg world mode (:width dimensions))
        nil))
    (.refresh screen)))
