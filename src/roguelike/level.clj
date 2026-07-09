(ns roguelike.level)

(def tile-types
  {:wall  {:type :wall         :glyph \#}
   :floor {:type :floor        :glyph \.}
   :door  {:type :closed-door  :glyph \+ :open? false :locked? false}})


;; level is a map of keys
;; :entities is a map of entity-id -> entity
;; :tiles is a vector of vectors (3x4 example):
;; [ [. . . .]
;;   [. . . .]
;;   [. . . .]]

(defn level-dimensions
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
  [y x])

(defn tile-at
  "Getter for tiles."
  [level [x y]]
  (get-in (:tiles level) (resolve-coords level [x y])))

(defn set-tile
  "Setter for tiles. Sets the tile in (level) at the provided coords ([x y]) using the provided function f.
  This is used for adding something new to the level at [x y]."
  [level [x y] tile]
  (let [[new-y new-x] (resolve-coords level [x y])]
    (assoc-in level [:tiles new-y new-x] tile)))

(defn update-tile
  "Updater for tiles. Updates the tile in (level) at the provided coords ([x y]) using the provided function f.
  This is used for altering the tile at [x y] in some way (ie. setting a door from closed to open)."
  [level [x y] f]
  (let [[new-y new-x] (resolve-coords level [x y])]
    (update-in level [:tiles new-y new-x] f)))

(defn is-passable?
  "Consumes a tile and returns a boolean telling whether the tile can be passed through or not."
  [tile]
  (case (:type tile)
    :wall        false
    :floor       true
    :closed-door (if (:open? tile) true false)))

(defn entity-at
  "Takes the level and gets the entity at the given pos or nil if there isn't one."
  [level [x y]]
  (let [entities (:entities level)]
    (some #(when (= (:pos %) [x y]) %) (vals entities))))

(defn get-entity
  [level entity-id]
  (get (:entities level) entity-id))

(defn add-entity
  "Places the given entity into the given level."
  [level entity]
  (assoc-in level [:entities (:id entity)] entity))

(defn remove-entity
  "Removes the entity associated with the given entity-id from the given level."
  [level entity-id]
  (update-in level [:entities] dissoc entity-id))

;; TODO: I don't think this is right
(defn update-entity
  "Uses the provided update function to update the entity with the given id in the given level."
  [level entity-id f]
  (update-in level [:entities entity-id] f))

(defn entities-of
  "Provides a vector of all the entities in the level."
  [level]
  (vals (:entities level)))

(defn test-level
  ([]
   (test-level 80 22))
  ([width height]
   (let [tiles (vec (for [y (range height)]
                      (vec (for [x (range width)]
                             (if (and (< y (dec height))
                                      (> y 0)
                                      (< x (dec width))
                                      (> x 0))
                               (:floor tile-types)
                               (:wall tile-types))))))]
     {:tiles tiles
      :entities {}})))  ;; TODO: update this to actually fill with monsters eventually

(defn level->tile-list
  "Converts a level into a list of tiles and their (x, y) coordinates. This allows the caller to view the level
  as a flat list and perform operations like map, reduce, etc."
  [level]
  (let [tiles (:tiles level)]
    (for [y (range (count tiles))
          x (range (count (tiles y)))
          :let [tile (tile-at level [x y])]]
      (assoc tile :x x :y y))))
