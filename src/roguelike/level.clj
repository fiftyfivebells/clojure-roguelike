(ns roguelike.level)

(def tile-types
  {:wall  {:type :wall         :glyph \#}
   :floor {:type :floor        :glyph \.}
   :door  {:type :closed-door  :glyph \+ :open? false :locked? false}})


;; level is a map of keys
;; (:tiles level) is a vector of vectors (3x4 example):
;; [ [. . . .]
;;   [. . . .]
;;   [. . . .]]

(defn- level-dimensions
  [level]
  (let [tiles (:tiles level)
        width (count (first tiles))
        height (count tiles)]
    [width height]))

(defn- in-bounds?
  [level  [x y]]
  (let [[width height] (level-dimensions level)]
    (and (< x width)
         (>= x 0)
         (< y height)
         (>= y 0))))

;; TODO: think about catching behavior somewhere (core.clj?)
(defn- assert-in-bounds!
  [level [x y]]
  (when (not (in-bounds? level [x y]))
    (let [[width height] (level-dimensions level)]
      (throw (ex-info "tile coordinates are out of bounds"
                      {:x x :y y :width width :height height})))))

(defn- resolve-coords
  "Translates the x,y coords received from callers into the row-major y,x representation that the level uses
  for its shape (a vector of row vectors)."
  [level [x y]]
  (assert-in-bounds! level [x y])
  [(:tiles level) y x])

(defn tile-at
  "Getter for tiles."
  [level [x y]]
  (get-in level (resolve-coords level [x y])))

(defn set-tile
  "Setter for tiles. Sets the tile in (level) at the provided coords ([x y]) using the provided function f.
  This is used for adding something new to the level at [x y]."
  [level [x y] tile]
  (assoc-in level (resolve-coords level [x y]) tile))

(defn update-tile
  "Updater for tiles. Updates the tile in (level) at the provided coords ([x y]) using the provided function f.
  This is used for altering the tile at [x y] in some way (ie. setting a door from closed to open)."
  [level [x y] f]
  (update-in level (resolve-coords level [x y]) f))

(defn is-passable?
  "Consumes a tile and returns a boolean telling whether the tile can be passed through or not."
  [tile]
  (case (:type tile)
    :wall        false
    :floor       true
    :closed-door (if (:open? tile) true false)))

(defn test-level
  []
  (let [tiles (vec (for [y (range 22)]
                     (vec (for [x (range 80)]
                            (if (and (< y 21) (> y 0) (< x 78) (> x 0))
                              (:floor tile-types)
                              (:wall tile-types))))))]
    {:tiles tiles}))

(defn level->tile-list
  "Converts a level into a list of tiles and their (x, y) coordinates. This allows the caller to view the level
  as a flat list and perform operations like map, reduce, etc."
  [level]
  (let [tiles (:tiles level)]
    (for [y (range (count tiles))
          x (range (count (tiles y)))
          :let [tile (tile-at level [x y])]]
      (assoc tile :x x :y y))))
