(ns roguelike.pathfinding
  (:require
   [roguelike.level :as level]))

(defn- neighbors
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (zero? dx) (zero? dy)))]
    [(+ x dx) (+ y dy)]))

(defn distance-map
  "BFS in the level outward from the provided source. Records how many steps it
   takes to reach each cell in the given level. When max-distance is given, the
   search stops expanding at that distance, so cells that far away or further
   are absent from the result instead of being recorded."
  ([walkable? src]
   (distance-map walkable? src nil))
  ([walkable? src max-distance]
   (loop [distances {src 0}
          frontier (conj clojure.lang.PersistentQueue/EMPTY src)]
     (if (empty? frontier)
       distances
       (let [cell (peek frontier) ;; this is an [x y] coord
             remaining-frontier (pop frontier) ;; rest of the queue (filled with [x y] coords)
             d (get distances cell)
             next-d (inc d)] ;; distance of this cell's neighbors from the source
         (if (and max-distance (>= next-d max-distance))
           (recur distances remaining-frontier)
           (let [[distances found]
                 (reduce (fn [[distances found] n]
                           (if (and (not (contains? distances n))
                                    (walkable? n))
                             [(assoc distances n next-d) (conj found n)]
                             [distances found]))
                         [distances []]
                         (neighbors cell))]
             (recur distances (into remaining-frontier found)))))))))
