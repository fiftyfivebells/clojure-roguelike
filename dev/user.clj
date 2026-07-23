(ns user
  (:require [roguelike.dungeon :as dungeon]
            [roguelike.level :as level]
            [roguelike.pathfinding :as pathfinding]))

(def ^:private classification->glyph
  {:floor       "."
   :wall        "#"
   :closed-door "+"
   :open-door   "'"
   :unknown     " "})

(defn print-generated-level
  "Generates a level with dungeon/generate and prints it to stdout as ASCII."
  ([] (print-generated-level 1 0))
  ([world-seed level-id]
   (let [lvl (dungeon/generate world-seed level-id)
         [width height] (level/dimensions lvl)]
     (doseq [y (range height)]
       (println (apply str (for [x (range width)]
                             (classification->glyph
                              (level/classify-tile (level/tile-at lvl [x y])))))))
     nil)))

(defn print-distance-map
  "Runs pathfinding/distance-map from src on lvl and prints the level with each
   reachable cell replaced by its distance (mod 10, so it lines up as a single
   character per cell). Unreachable passable cells and walls print as their
   normal glyph."
  [lvl src]
  (let [walkable? (partial level/walkable-at? lvl)
        [width height] (level/dimensions lvl)
        distances (pathfinding/distance-map walkable? src)]
    (doseq [y (range height)]
      (println (apply str (for [x (range width)]
                            (if-let [d (get distances [x y])]
                              (str (mod d 10))
                              (classification->glyph
                               (level/classify-tile (level/tile-at lvl [x y]))))))))
    distances))
