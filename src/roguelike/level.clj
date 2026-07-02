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

(defn tile-at
  [level [x y]]
  (assert-in-bounds! level [x y])
  (get-in level [:tiles y x]))

(defn set-tile
  [level [x y] tile]
  (assert-in-bounds! level [x y])
  (assoc-in level [:tiles y x] tile))

(defn update-tile
  [level [x y] f]
  (assert-in-bounds! level [x y])
  (update-in level [:tiles y x] f))

(defn is-passable?
  [tile]
  (case (:type tile)
    :wall        false
    :floor       true
    :closed-door (if (:open? tile) true false)))

(defn test-level
  []
  (let [tiles (vec (for [y (range 22)]
                     (vec (for [x (range 79)]
                            (if (and (< y 21) (> y 0) (< x 78) (> x 0))
                              (:floor tile-types)
                              (:wall tile-types))))))]
    {:tiles tiles}))

(defn level->tile-list
  [level]
  (let [tiles (:tiles level)]
    (for [y (range (count tiles))
          x (range (count (tiles y)))
          :let [tile (tile-at level [x y])]]
      (assoc tile :x x :y y))))
