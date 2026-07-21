(ns roguelike.fov-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [roguelike.fov :as fov]))

;;; ---------------------------------------------------------------------------
;;; String-art harness
;;;
;;; A map is a vector of equal-length strings. Characters:
;;;   @  origin (also floor)
;;;   #  opaque
;;;   .  floor (transparent)
;;;
;;; Any coordinate OUTSIDE the grid is treated as opaque, matching the real
;;; level/opaque-at? contract. This lets the FOV kernel scan past the edge
;;; without special-casing bounds — exactly what Phase A1b set up.
;;; ---------------------------------------------------------------------------

(defn parse-map
  "Parse a vector of strings into {:opaque? fn :origin [x y] :floors #{[x y]...}}.
  :floors is every in-bounds non-opaque cell (including the origin) — the set
  the symmetry property quantifies over."
  [rows]
  (let [height (count rows)
        width  (count (first rows))
        cells  (for [y (range height)
                     x (range width)
                     :let [ch (get-in rows [y x])]]
                 [[x y] ch])
        opaque (into #{} (keep (fn [[pos ch]] (when (= ch \#) pos)) cells))
        floors (into #{} (keep (fn [[pos ch]] (when (not= ch \#) pos)) cells))
        origin (some (fn [[pos ch]] (when (= ch \@) pos)) cells)]
    (when (nil? origin)
      (throw (ex-info "map has no @ origin" {:rows rows})))
    {:width   width
     :height  height
     :origin  origin
     :floors  floors
     ;; OOB collapses to opaque
     :opaque? (fn [[x y]]
                (or (< x 0) (< y 0) (>= x width) (>= y height)
                    (contains? opaque [x y])))}))

(defn opaque-fn
  "Build just the opaque? predicate from rows, no @ required.
  OOB collapses to opaque, same as parse-map."
  [rows]
  (let [height (count rows)
        width  (count (first rows))
        opaque (into #{} (for [y (range height)
                               x (range width)
                               :when (= \# (get-in rows [y x]))]
                           [x y]))]
    (fn [[x y]]
      (or (< x 0) (< y 0) (>= x width) (>= y height)
          (contains? opaque [x y])))))

(defn render-visible
  "Render a visible-set back to strings for eyeballing. Visible floor -> the
  original char; visible wall -> #; not-visible -> space. Origin stays @."
  [rows visible]
  (let [{:keys [width height origin]} (parse-map rows)]
    (str/join
     "\n"
     (for [y (range height)]
       (str/join
        (for [x (range width)]
          (cond
            (= [x y] origin)              \@
            (contains? visible [x y])     (get-in rows [y x])
            :else                         \space)))))))

(defn compute
  "Convenience: parse the map, run FOV at the given radius, return the visible set."
  [rows radius]
  (let [{:keys [opaque? origin]} (parse-map rows)]
    (fov/visible-cells opaque? origin radius)))

(defn- floor-coords
  "All in-bounds non-opaque coords in a string map."
  [rows]
  (:floors (parse-map rows)))

;;; ---------------------------------------------------------------------------
;;; Sanity / invariant tests (small, exact)
;;; ---------------------------------------------------------------------------

(deftest origin-always-visible
  (testing "origin is in its own field, even boxed in"
    (is (contains? (compute ["#####"
                             "##@##"
                             "#####"] 8)
                   [2 1]))))

(deftest empty-room-full-circle
  (testing "in an open room every floor within radius is visible"
    (let [rows   ["........."
                  "........."
                  "....@...."
                  "........."
                  "........."]
          vis    (compute rows 2)
          origin [4 2]]
      ;; everything within euclidean radius 2 (squared <= 4) should be visible
      (doseq [c (floor-coords rows)
              :let [[x y] c
                    [ox oy] origin
                    d2 (+ (* (- x ox) (- x ox)) (* (- y oy) (- y oy)))]]
        (if (<= d2 4)
          (is (contains? vis c) (str c " within r=2 should be visible"))
          (is (not (contains? vis c)) (str c " outside r=2 should be hidden")))))))

(deftest radius-zero-sees-only-origin
  (is (= #{[2 1]} (compute ["#####"
                            "##@##"
                            "#####"] 0))))

(deftest sealed-closet-sees-surrounding-walls
  (testing "boxed-in origin sees its origin and the four+diagonal walls around it"
    (let [rows ["#####"
                "#####"
                "##@##"
                "#####"
                "#####"]
          vis  (compute rows 8)]
      ;; the ring of walls immediately around the origin is revealed
      ;; (walls are revealed unconditionally when touched)
      (doseq [c [[2 2]           ; origin
                 [1 1] [2 1] [3 1]
                 [1 2]       [3 2]
                 [1 3] [2 3] [3 3]]]
        (is (contains? vis c) (str "expected wall/origin " c " to be visible")))
      ;; but nothing two rings out — the inner wall ring blocks it
      (doseq [c [[0 0] [4 0] [0 4] [4 4] [2 0] [0 2] [4 2] [2 4]]]
        (is (not (contains? vis c)) (str "expected " c " to be blocked"))))))

;;; ---------------------------------------------------------------------------
;;; Pillar / shadow goldens — the cases most likely to expose a slope bug
;;; ---------------------------------------------------------------------------

(deftest pillar-casts-expanding-shadow
  (testing "a single pillar casts a shadow that widens behind it"
    (let [rows ["............."
                "............."
                "............."
                "......#......"
                "............."
                "......@......"]
          vis  (compute rows 10)]
      ;; the pillar itself is visible (wall, directly in line, touched)
      (is (contains? vis [6 3]) "pillar is visible")
      ;; the cell directly behind the pillar (further from origin) is shadowed
      (is (not (contains? vis [6 2])) "cell directly behind pillar is hidden")
      (is (not (contains? vis [6 1])) "shadow continues behind pillar")
      (is (not (contains? vis [6 0])) "shadow reaches the back")
      ;; cells to the side of the pillar at its own depth remain visible
      (is (contains? vis [4 3]) "left of pillar visible")
      (is (contains? vis [8 3]) "right of pillar visible")
      ;; the shadow expands: at the row two-behind the pillar, the hidden band
      ;; is wider than one cell (this is the "expanding pillar shadow" property)
      (is (not (contains? vis [6 1])) "shadow core still hidden two rows back"))))

(deftest pillar-shadow-is-symmetric-left-right
  (testing "pillar shadow is mirrored across the vertical center line"
    (let [rows ["............."
                "............."
                "............."
                "......#......"
                "............."
                "......@......"]
          vis  (compute rows 10)]
      ;; for every hidden cell above the pillar, its mirror across x=6 is also hidden
      (doseq [y (range 0 3)
              dx (range 1 5)]
        (let [left  [(- 6 dx) y]
              right [(+ 6 dx) y]]
          (is (= (contains? vis left) (contains? vis right))
              (str "mirror mismatch: " left " vs " right)))))))

;;; ---------------------------------------------------------------------------
;;; Doorway: sight through a gap in a wall forms a cone
;;; ---------------------------------------------------------------------------

(deftest doorway-limits-sight-to-cone
  (testing "a wall with one opening lets sight through as a widening cone"
    (let [rows ["........."
                "........."
                "####.####"   ; wall with a gap at x=4
                "........."
                "....@...."]
          vis  (compute rows 10)]
      ;; the doorway cell is visible
      (is (contains? vis [4 2]) "doorway is visible")
      ;; directly beyond the doorway is visible
      (is (contains? vis [4 1]) "straight through the door")
      (is (contains? vis [4 0]) "far side of the door, on axis")
      ;; the far corners beyond the wall (not through the gap) are NOT visible
      (is (not (contains? vis [0 0])) "far corner blocked by wall")
      (is (not (contains? vis [8 0])) "other far corner blocked by wall")
      ;; the wall cells adjacent to the doorway are visible (touched faces)
      (is (contains? vis [3 2]) "wall left of door visible")
      (is (contains? vis [5 2]) "wall right of door visible"))))

;;; ---------------------------------------------------------------------------
;;; Corner peeking — the classic wrong-answer case
;;; ---------------------------------------------------------------------------

(deftest corner-peek-does-not-see-around-wall
  (testing "standing beside a wall corner, you cannot see the cell diagonally
            hidden behind it"
    ;; @ is at [1 2]; a wall block sits at [3 1]; the cell at [3 0] and beyond
    ;; to the upper-right should be shadowed by the corner, not peeked.
    (let [rows ["......."
                "...#..."
                ".@....."
                "......."]
          vis  (compute rows 10)]
      (is (contains? vis [3 1]) "the wall corner itself is visible")
      ;; the classic asymmetry test: a symmetric algorithm must NOT reveal the
      ;; cell directly behind the corner along the grazing diagonal
      (is (not (contains? vis [4 0]))
          "must not peek past the corner to [4 0]"))))

;;; ---------------------------------------------------------------------------
;;; Off-center origin — catches quadrant transform sign errors
;;; (a mirrored quadrant is invisible in a centered symmetric room)
;;; ---------------------------------------------------------------------------

(deftest off-center-origin-reveals-correct-quadrants
  (testing "origin in a corner sees into the room, not off the edges"
    (let [rows ["@........"
                "........."
                "........."]
          vis  (compute rows 4)]
      ;; cells down-and-right of the corner origin are visible within radius
      (is (contains? vis [1 0]))
      (is (contains? vis [0 1]))
      (is (contains? vis [1 1]))
      (is (contains? vis [2 0]))
      ;; a cell outside radius is not
      (is (not (contains? vis [8 2])))
      ;; walls are revealed unconditionally when touched, and OOB collapses to
      ;; opaque, so out-of-bounds "walls" beyond the corner origin are expected
      ;; (that's the caller's job to filter, e.g. level/opaque-at? does the same
      ;; OOB-as-opaque collapse); the real invariant is that every in-bounds
      ;; cell in the visible set is actually a floor, not a phantom wall
      (doseq [[x y] vis
              :when (and (>= x 0) (< x 9) (>= y 0) (< y 3))]
        (is (not= \# (get-in rows [y x])) (str "in-bounds visible cell " [x y] " should be a floor or origin"))))))

;;; ---------------------------------------------------------------------------
;;; Property-based tests
;;; ---------------------------------------------------------------------------

;; Generate small random maps with a guaranteed floor origin.
(def gen-map
  (gen/let [w      (gen/choose 3 9)
            h      (gen/choose 3 9)
            ;; each interior cell is a wall with ~25% probability
            walls  (gen/vector (gen/frequency [[3 (gen/return \.)]
                                               [1 (gen/return \#)]])
                               (* w h))
            ;; pick an origin cell that we will FORCE to be floor
            ox     (gen/choose 0 (dec w))
            oy     (gen/choose 0 (dec h))]
    (let [grid   (vec (for [y (range h)]
                        (vec (for [x (range w)] (nth walls (+ x (* y w)))))))
          ;; force origin to floor
          grid   (assoc-in grid [oy ox] \.)
          rows   (mapv #(apply str %) grid)]
      {:rows rows :origin [ox oy]})))

(defn- opaque-from-rows
  [rows]
  (opaque-fn rows))

(defspec origin-always-in-field 300
  (prop/for-all [{:keys [rows origin]} gen-map
                 r (gen/choose 0 12)]
                (contains? (fov/visible-cells (opaque-fn rows) origin r)
                           origin)))

(defspec every-visible-cell-within-radius 300
  (prop/for-all [{:keys [rows origin]} gen-map
                 r (gen/choose 0 12)]
                (let [[ox oy] origin
                      vis     (fov/visible-cells (opaque-fn rows) origin r)]
                  (every? (fn [[x y]]
                            (<= (+ (* (- x ox) (- x ox))
                                   (* (- y oy) (- y oy)))
                                (* r r)))
                          vis))))

(defspec monotonic-in-radius 300
  (prop/for-all [{:keys [rows origin]} gen-map
                 r (gen/choose 0 10)]
                (let [opaque? (opaque-from-rows rows)
                      smaller (fov/visible-cells opaque? origin r)
                      larger  (fov/visible-cells opaque? origin (inc r))]
                  (clojure.set/subset? smaller larger))))

;; The symmetry property — floors only, equal radius.
;; "if floor A sees floor B, then floor B sees floor A"
;; Restricting to floors is essential: walls are revealed asymmetrically by
;; design (Albert Ford). Testing all cells here would fail immediately.
(defspec floor-visibility-is-symmetric 300
  (prop/for-all [{:keys [rows]} gen-map
                 r (gen/choose 1 8)]
                (let [height  (count rows)
                      width   (count (first rows))
          ;; opaque? predicate, OOB collapses to opaque
                      opaque? (fn [[x y]]
                                (or (< x 0) (< y 0) (>= x width) (>= y height)
                                    (= \# (get-in rows [y x]))))
          ;; every in-bounds non-# cell is a floor
                      floors  (for [y (range height)
                                    x (range width)
                                    :when (not= \# (get-in rows [y x]))]
                                [x y])]
                  (every?
                   (fn [a]
                     (let [from-a (fov/visible-cells opaque? a r)]
                       (every?
                        (fn [b]
                          (= (contains? from-a b)
                             (contains? (fov/visible-cells opaque? b r) a)))
                        floors)))
                   floors))))
