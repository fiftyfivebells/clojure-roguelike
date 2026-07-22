(ns roguelike.level
  (:require [roguelike.knowledge :as knowledge]))

;; level is a map of keys
;; :entities is a map of entity-id -> entity
;; :tiles is a vector of vectors (3x4 example):
;; [ [. . . .]
;;   [. . . .]
;;   [. . . .]]

;; Bounds

(defn dimensions
  [level]
  (let [tiles (:tiles level)
        width (count (first tiles))
        height (count tiles)]
    [width height]))

(defn- in-bounds?
  [level  [x y]]
  (let [[width height] (dimensions level)]
    (and (< x width)
         (>= x 0)
         (< y height)
         (>= y 0))))

;; TODO: think about catching behavior somewhere (core.clj?)
(defn- assert-in-bounds!
  [level [x y]]
  (when (not (in-bounds? level [x y]))
    (let [[width height] (dimensions level)]
      (throw (ex-info "tile coordinates are out of bounds"
                      {:x x :y y :width width :height height})))))

(defn- resolve-coords
  "Translates the x,y coords received from callers into the row-major y,x
   representation that the level uses for its shape (a vector of row vectors)."
  [level [x y]]
  (assert-in-bounds! level [x y])
  [y x])

;; Tile functions
;; a tile is a map. for now it looks like this:
;; {:tile/type :wall}

(defn tile-at
  "Getter for tiles."
  [level [x y]]
  (get-in (:tiles level) (resolve-coords level [x y])))

(defn set-tile
  "Setter for tiles. Sets the tile in (level) at the provided coords ([x y]).
   This is used for adding something new to the level at [x y]."
  [level [x y] tile]
  (let [[new-y new-x] (resolve-coords level [x y])]
    (assoc-in level [:tiles new-y new-x] tile)))

(defn update-tile
  "Updater for tiles. Updates the tile in (level) at the provided coords ([x y])
   using the provided function f. This is used for altering the tile at [x y] in
   some way (ie. setting a door from closed to open)."
  [level [x y] f]
  (let [[new-y new-x] (resolve-coords level [x y])]
    (update-in level [:tiles new-y new-x] f)))

(def ^:private passable-classifications #{:floor :open-door})
(def ^:private transparent-classification #{:floor :open-door})

(defn classify-tile
  "Returns the semantic classification of a tile: :floor, :wall, :closed-door,
   :open-door, or :unknown for an unrecognized tile type. This should be the one
   place that translates a tile's raw :tile/type into the vocabulary callers
   should build on."
  [tile]
  (case (:tile/type tile)
    :wall        :wall
    :floor       :floor
    :closed-door (if (:open? tile) :open-door :closed-door)
    :unknown))

(defn passable?
  "Consumes a tile and returns a boolean telling whether the tile can be passed
   through or not."
  [tile]
  (contains? passable-classifications (classify-tile tile)))

(defn transparent?
  "Consumes a tile and returns a boolean telling whether the tile can be seen through or not. A
  transparent tile is not necessarily also passable."
  [tile]
  (contains? transparent-classification (classify-tile tile)))

(defn opaque-at?
  "Determines whether to the tile is opaque or visible. Tiles that are out of
   bounds or behind a tile that is not transparent are considered opaque."
  [level [x y]]
  (or (not (in-bounds? level [x y]))
      (not (transparent? (tile-at level [x y])))))

;; Entity functions

;; TODO: keep an eye on sluggishness during turn updates. This is a scan of all entities every time.
;; it could wind up being slow and needing tuning (maybe a stored pos->id map?)
(defn entity-at
  "Takes the level and gets the entity at the given pos or nil if there isn't
   one."
  [level [x y]]
  (let [entities (:entities level)]
    (some #(when (= (:pos %) [x y]) %) (vals entities))))

(defn get-entity
  [level entity-id]
  (get (:entities level) entity-id))

(defn add-entity
  "Places the given entity into the given level."
  [level entity]
  (assoc-in level [:entities (:entity/id entity)] entity))

(defn- assert-entity-exists!
  [level entity-id]
  (when (nil? (get-entity level entity-id))
    (throw (ex-info "provided entity id does not exist"
                    {:entity/id entity-id}))))

(defn remove-entity
  "Removes the entity associated with the given entity-id from the given level."
  [level entity-id]
  (assert-entity-exists! level entity-id)
  (update-in level [:entities] dissoc entity-id))

(defn update-entity
  "Uses the provided update function to update the entity with the given id in
   the given level."
  [level entity-id f]
  (assert-entity-exists! level entity-id)
  (update-in level [:entities entity-id] f))

(defn entities-of
  "Provides a vector of all the entities in the level."
  [level]
  (vals (:entities level)))

;; Knowledge/FOV

(defn remember-visible
  "Takes in a list of visible tiles and folds them into the level's known tile
   list. Uses knowledge/remember to 'remember' each tile."
  [level visible]
  (let [remember (fn [acc cell]
                   (knowledge/remember acc (tile-at level cell) cell))]
    (update level :known
            (fn [known]
              (reduce remember known visible)))))

(defn known-tiles
  [level]
  (get level :known))

;; Construction

(def ^:private tile-types
  {:wall  {:tile/type :wall}
   :floor {:tile/type :floor}
   :door  {:tile/type :door :open? false :locked? false}})

(defn floor
  []
  (:floor tile-types))

(defn wall
  []
  (:wall tile-types))

(defn door
  ([] (:door tile-types))
  ([{:keys [open? locked?] :or {open? false locked? false}}]
   (merge (:door tile-types) {:open? open? :locked? locked?})))

(defn set-rooms
  [level rooms]
  (assoc-in level [:rooms] rooms))

(defn solid-level
  ([]
   (solid-level 80 22))
  ([width height]
   (let [tiles (vec (for [y (range height)]
                      (vec (for [x (range width)]
                             (:wall tile-types)))))]
     {:tiles tiles
      :entities {}
      :known (knowledge/empty-knowledge)})))

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
      :entities {}
      :known (knowledge/empty-knowledge)})))  ;; TODO: update this to actually fill with monsters eventually

;; Traversal

(defn level->tile-list
  "Converts a level into a list of tiles and their (x, y) coordinates. This
   allows the caller to view the level as a flat list and perform operations
   like map, reduce, etc."
  [level]
  (for [[y row] (map-indexed vector (:tiles level))
        [x tile] (map-indexed vector row)]
    (assoc tile :pos [x y])))
