(ns roguelike.dungeon)

(def tile-types
  {:wall  {:type :wall         :glyph \#}
   :floor {:type :floor        :glyph \.}
   :door  {:type :closed-door  :glyph \+ :open? false :locked? false}})

(defn get-tile
  [level [x y]]
  (get-in level [x y]))

(defn is-passable?
  [tile]
  (case (:type tile)
    :wall        false
    :floor       true
    :closed-door (if (:open? tile) true false)))

(defn test-level
  []
  (let [rows (for [y (range 22)]
               (vec (for [x (range 79)]
                      (if (and (< y 21) (> y 0) (< x 78) (> x 0))
                        (:floor tile-types)
                        (:wall tile-types)))))]
    (into [] rows)))

(defn level->tile-list
  [level]
  (for [row (range (count level))
        col (range (count (level row)))
        :let [tile (get-in level [row col])]]
    (assoc tile :x col :y row)))
