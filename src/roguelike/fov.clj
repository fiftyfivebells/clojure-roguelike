(ns roguelike.fov
  (:require [clojure.math :as math]))

;; Symmetric shadowcasting — a pure field-of-view algorithm.
;;
;; Based on Albert Ford's "Symmetric Shadowcasting":
;; - https://www.albertford.com/shadowcasting/
;; - reference implementation:
;; https://github.com/370417/symmetric-shadowcasting
;;
;; Builds on the six FOV properties from Adam Milazzo's "Roguelike Vision
;; Algorithms":
;; - http://www.adammil.net/blog/v125_Roguelike_Vision_Algorithms.html
;;
;; Key property (enforced by symmetric?): floor-tile visibility is perfectly
;; symmetric, ie if floor A sees floor B, then B sees A. This does NOT hold for
;; wall tiles, which are which are revealed regardless. Tests must restrict the
;; symmetry property to non-opaque cells at equal radius.

;; Helpers

(defn- slope
  "Calculates the slope through the near edge of the tile (depth, col). 'col' is
   the sideways distance from the (0, 0) point, and 'depth' is the 'height' of
   the distance from (0, 0). Returns num and den. The den value will always be
   positive because depth is always > 0."
  [depth col]
  [(dec (* 2 col)) (* 2 depth)])

(defn- symmetric?
  "Is the center of the tile at (depth, col) within [start-slope, end-slope]?
   Decides whether a floor tile gets revealed."
  [depth col [start-num start-den] [end-num end-den]]
  (and (>= (* col start-den) (* depth start-num))
       (<= (* col end-den) (* depth end-num))))

(defn- within-radius?
  "Euclidean distance check: compares depth and col to radius so we get a round
   field instead of a square."
  [depth col radius]
  (<= (+ (* depth depth) (* col col)) (* radius radius)))

(defn- row-cols
  "Takes the depth and the start/end slopes, then returns the min column and max
   column inclusive range of tiles to scan at the provided depth. The min column
   rounds ties up and the max column rounds ties down, so that a slope landing
   exactly on a tile boundary is resolved the same way regardless of scan
   direction — this is what keeps floor visibility symmetric."
  [depth [start-num start-den] [end-num end-den]]
  [(math/floor-div (+ (* 2 depth start-num) start-den) (* 2 start-den))
   (- (math/floor-div (- end-den (* 2 depth end-num)) (* 2 end-den)))])

(defn transform
  "Maps a quadrant (depth, col) to a world [x y] coordinate when given an origin
 (x, y)."
  [cardinal [ox oy] depth col]
  (case cardinal
    :north [(+ ox col) (- oy depth)]
    :south [(+ ox col) (+ oy depth)]
    :east [(+ ox depth) (+ oy col)]
    :west [(- ox depth) (+ oy col)]))

;; Symmetric shadow-casting implementation

(defn- scan
  "Using an opaque? function, a starting point, and a cardinal direction (along
   with radius, depth, starting and ending slopes), returns a set of the visible
   coordinates from the origin point."
  [opaque? cardinal origin radius depth start-slope end-slope visible]
  (if (> depth radius)
    visible
    (let [[min-col max-col] (row-cols depth start-slope end-slope)]
      (loop [col min-col
             start-slope start-slope
             prev :none
             visible visible]
        (if (> col max-col)
          (if (= prev :floor)
            (scan opaque? cardinal origin radius (inc depth) start-slope end-slope visible)
            visible)
          (let [cell (transform cardinal origin depth col)
                wall? (opaque? cell)
                this (if wall? :wall :floor)
                ;; walls always reveal, floors only reveal if they're
                ;; symmetric (center of tile within slopes)
                visible
                (if (and (or wall? (symmetric? depth col start-slope end-slope))
                         (within-radius? depth col radius))
                  (conj visible cell)
                  visible)
                start-slope (if (and (= prev :wall) (= this :floor))
                              (slope depth col)
                              start-slope)
                ;; floor -> wall triggers a narrower sub-scan (new
                ;; recursion, NOT recur)
                visible (if (and (= prev :floor) (= this :wall))
                          (scan opaque? cardinal origin radius (inc depth) start-slope (slope depth col) visible)
                          visible)]
            (recur (inc col) start-slope this visible)))))))

(defn visible-cells
  "Symmetric shadow-casting. opaque? is [x y] -> boolean, should always return
   true or false (no throws). Scans all four cardinal directions and returns a
   set of all of the coordinates visible from the origin."
  [opaque? origin radius]
  (let [scanner (fn [visible cardinal]
                  (scan opaque? cardinal origin radius 1 [-1 1] [1 1] visible))
        initial #{origin}
        cardinal [:north :south :east :west]]
    (reduce scanner initial cardinal)))
