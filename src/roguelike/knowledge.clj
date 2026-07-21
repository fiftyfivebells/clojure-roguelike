(ns roguelike.knowledge)

(defn empty-knowledge
  "Constructor: creates an empty knowledge map."
  []
  {})

(defn remember
  "Adds tile to the knowledge map at [x y]. This will overwrite whatever was in
   the map at these coordinates."
  [known tile [x y]]
  (assoc known [x y] tile))

(defn remembered-tile
  "Returns the tile stored at these coordinates in the knowledge map, or gives
   nil if nothing is there."
  [known [x y]]
  (get known [x y]))

(defn seen?
  "Boolean value "
  [known [x y]]
  (contains? known [x y]))
