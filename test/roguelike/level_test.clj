(ns roguelike.level-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [roguelike.level :as level]))

;;; Helpers

(defn- make-level
  "Build a level whose :tiles is a height x width grid filled with floor tiles."
  [width height]
  {:tiles (vec (for [_ (range height)]
                 (vec (repeat width (level/floor)))))})

(defn- make-level-filled
  "Build a level where every tile is f (a tile map)."
  [width height tile]
  {:tiles (vec (for [_ (range height)]
                 (vec (repeat width tile))))})

(def door (level/door))

;;; tile-at

(deftest tile-at-returns-correct-tile
  (let [lvl (make-level 5 4)]
    (is (= (level/floor) (level/tile-at lvl [0 0])))
    (is (= (level/floor) (level/tile-at lvl [4 3])))))

(deftest tile-at-distinguishes-position
  (let [lvl (-> (make-level-filled 5 4 (level/wall))
                (level/set-tile [2 1] (level/floor)))]
    (is (= (level/floor) (level/tile-at lvl [2 1])))
    (is (= (level/wall)  (level/tile-at lvl [0 0])))))

(deftest tile-at-out-of-bounds-throws
  (let [lvl (make-level 5 4)]
    (is (thrown? clojure.lang.ExceptionInfo (level/tile-at lvl [-1 0])))
    (is (thrown? clojure.lang.ExceptionInfo (level/tile-at lvl [0 -1])))
    (is (thrown? clojure.lang.ExceptionInfo (level/tile-at lvl [5 0])))
    (is (thrown? clojure.lang.ExceptionInfo (level/tile-at lvl [0 4])))))

(deftest tile-at-exception-carries-coords
  (let [lvl (make-level 3 3)]
    (try
      (level/tile-at lvl [9 9])
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e)]
          (is (= 9 (:x d)))
          (is (= 9 (:y d)))
          (is (= 3 (:width d)))
          (is (= 3 (:height d))))))))

;;; set-tile

(deftest set-tile-replaces-tile
  (let [lvl    (make-level 5 4)
        result (level/set-tile lvl [1 2] (level/wall))]
    (is (= (level/wall)  (level/tile-at result [1 2])))
    (is (= (level/floor) (level/tile-at lvl   [1 2])) "original is unchanged")))

(deftest set-tile-only-affects-target-cell
  (let [lvl    (make-level 3 3)
        result (level/set-tile lvl [1 1] (level/wall))]
    (doseq [x (range 3)
            y (range 3)
            :when (not= [x y] [1 1])]
      (is (= (level/floor) (level/tile-at result [x y]))))))

(deftest set-tile-out-of-bounds-throws
  (let [lvl (make-level 5 4)]
    (is (thrown? clojure.lang.ExceptionInfo (level/set-tile lvl [5 0] (level/wall))))
    (is (thrown? clojure.lang.ExceptionInfo (level/set-tile lvl [0 4] (level/wall))))))

;;; update-tile

(deftest update-tile-applies-function
  (let [lvl    (make-level 3 3)
        result (level/update-tile lvl [0 0] (fn [_] (level/wall)))]
    (is (= (level/wall) (level/tile-at result [0 0])))))

(deftest update-tile-receives-current-tile
  (let [lvl    (make-level-filled 3 3 door)
        seen   (atom nil)
        _      (level/update-tile lvl [1 1] (fn [t] (reset! seen t) t))]
    (is (= door @seen))))

(deftest update-tile-out-of-bounds-throws
  (let [lvl (make-level 5 4)]
    (is (thrown? clojure.lang.ExceptionInfo (level/update-tile lvl [-1 0] identity)))
    (is (thrown? clojure.lang.ExceptionInfo (level/update-tile lvl [0 -1] identity)))))

;;; is-passable?

(deftest is-passable-wall
  (is (false? (level/passable? (level/wall)))))

(deftest is-passable-floor
  (is (true? (level/passable? (level/floor)))))

(deftest is-passable-closed-door
  (is (false? (level/passable? door))))

(deftest is-passable-open-door
  (is (true? (level/passable? (assoc door :open? true)))))

;;; is-transparent?

(deftest is-transparent-wall
  (is (false? (level/transparent? (level/wall)))))

(deftest is-transparent-floor
  (is (true? (level/transparent? (level/floor)))))

(deftest is-transparent-closed-door
  (is (false? (level/transparent? door))))

(deftest is-transparent-open-door
  (is (true? (level/transparent? (assoc door :open? true)))))

;;; opaque-at?

(deftest is-opaque-out-of-bounds-and-wall
  (let [lvl (make-level-filled 5 4 (level/wall))]
    (is (true? (level/opaque-at? lvl [6 6])))
    (is (true? (level/opaque-at? lvl [0 0])))))

;;; Property-based tests

(def gen-dims
  "Generates [width height] pairs."
  (gen/bind (gen/choose 1 20)
            (fn [w] (gen/fmap (fn [h] [w h]) (gen/choose 1 20)))))

(def gen-level-with-coord
  "Generates {:w :h :x :y} where [x y] is in-bounds."
  (gen/bind gen-dims
            (fn [[w h]]
              (gen/bind (gen/choose 0 (dec w))
                        (fn [x]
                          (gen/fmap (fn [y] {:w w :h h :x x :y y})
                                    (gen/choose 0 (dec h))))))))

(defspec set-then-get-roundtrips 200
  (prop/for-all [{:keys [w h x y]} gen-level-with-coord]
                (let [lvl    (make-level w h)
                      result (level/set-tile lvl [x y] (level/wall))]
                  (= (level/wall) (level/tile-at result [x y])))))

(def gen-non-square-level-with-coord
  (gen/bind (gen/such-that (fn [[w h]] (not= w h)) gen-dims)
            (fn [[w h]]
              (gen/bind (gen/choose 0 (dec w))
                        (fn [x]
                          (gen/fmap (fn [y] {:w w :h h :x x :y y})
                                    (gen/choose 0 (dec h))))))))

(defspec set-then-get-roundtrips-non-square 200
  (prop/for-all [{:keys [w h x y]} gen-non-square-level-with-coord]
                (let [lvl    (make-level w h)
                      result (level/set-tile lvl [x y] (level/wall))]
                  (= (level/wall) (level/tile-at result [x y])))))

(def gen-level-with-oob-coord
  "Generates {:w :h :x :y} where [x y] is guaranteed out-of-bounds."
  (gen/bind gen-dims
            (fn [[w h]]
              (gen/fmap
               (fn [[side offset]]
                 (case side
                   :neg-x {:w w :h h :x (- (inc offset)) :y 0}
                   :neg-y {:w w :h h :x 0 :y (- (inc offset))}
                   :over-x {:w w :h h :x (+ w offset) :y 0}
                   :over-y {:w w :h h :x 0 :y (+ h offset)}))
               (gen/tuple (gen/elements [:neg-x :neg-y :over-x :over-y])
                          (gen/choose 0 10))))))

(defspec out-of-bounds-always-throws 200
  (prop/for-all [{:keys [w h x y]} gen-level-with-oob-coord]
                (try
                  (level/tile-at (make-level w h) [x y])
                  false
                  (catch clojure.lang.ExceptionInfo _ true))))

(defspec in-bounds-never-throws 200
  (prop/for-all [[w h] gen-dims]
                (let [lvl (make-level w h)]
                  (every? (fn [[x y]] (= (level/floor) (level/tile-at lvl [x y])))
                          (for [x (range w) y (range h)] [x y])))))

(defspec set-tile-is-pure 200
  (prop/for-all [[w h] gen-dims]
                (let [lvl (make-level w h)
                      _   (level/set-tile lvl [0 0] (level/wall))]
                  (= (level/floor) (level/tile-at lvl [0 0])))))

(defspec update-tile-identity-is-noop 200
  (prop/for-all [{:keys [w h x y]} gen-level-with-coord]
                (let [lvl    (make-level w h)
                      result (level/update-tile lvl [x y] identity)]
                  (= (level/tile-at lvl [x y])
                     (level/tile-at result [x y])))))

;;; Room lighting

(defn- make-room
  [[rx ry rw rh] & {:keys [lit?] :or {lit? false}}]
  {:bounds [rx ry rw rh] :lit? lit?})

(def ^:private room-a (make-room [1 1 3 3]))   ; covers x[1,4) y[1,4)
(def ^:private room-b (make-room [10 10 2 2])) ; covers x[10,12) y[10,12)

(defn- level-with-rooms
  [rooms]
  (level/set-rooms (make-level 20 20) rooms))

(deftest point-in-room-true-inside-bounds
  (is (level/point-in-room? room-a [1 1]))
  (is (level/point-in-room? room-a [3 3])))

(deftest point-in-room-false-outside-bounds
  (is (not (level/point-in-room? room-a [4 3]))) ; right edge is exclusive
  (is (not (level/point-in-room? room-a [1 4]))) ; bottom edge is exclusive
  (is (not (level/point-in-room? room-a [0 0]))))

(deftest room-tiles-covers-full-bounds
  (is (= (set (for [x [1 2 3] y [1 2 3]] [x y]))
         (set (level/room-tiles room-a)))))

(deftest room-glow-tiles-includes-surrounding-wall-ring
  (is (= (set (for [x [0 1 2 3 4] y [0 1 2 3 4]] [x y]))
         (set (level/room-glow-tiles room-a)))))

(deftest room-at-finds-containing-room
  (let [lvl (level-with-rooms [room-a room-b])]
    (is (= room-a (level/room-at lvl [2 2])))
    (is (= room-b (level/room-at lvl [10 10])))))

(deftest room-at-nil-outside-any-room
  (let [lvl (level-with-rooms [room-a room-b])]
    (is (nil? (level/room-at lvl [5 5])))))

(deftest light-room-at-marks-containing-room-lit
  (let [lvl (level-with-rooms [room-a room-b])
        lit (level/light-room-at lvl [2 2])]
    (is (:lit? (level/room-at lit [2 2])))
    (is (not (:lit? (level/room-at lit [10 10]))))))

(deftest light-room-at-noop-outside-any-room
  (let [lvl (level-with-rooms [room-a room-b])
        result (level/light-room-at lvl [5 5])]
    (is (= (level/get-rooms lvl) (level/get-rooms result)))))

(deftest light-room-at-is-idempotent
  (let [lvl (level-with-rooms [room-a room-b])
        once (level/light-room-at lvl [2 2])
        twice (level/light-room-at once [2 2])]
    (is (= (level/get-rooms once) (level/get-rooms twice)))))

(deftest lit-tiles-empty-when-no-room-lit
  (let [lvl (level-with-rooms [room-a room-b])]
    (is (empty? (level/lit-tiles lvl)))))

(deftest lit-tiles-only-includes-lit-rooms
  (let [lvl (level-with-rooms [(make-room [1 1 3 3] :lit? true) room-b])]
    (is (= (set (level/room-glow-tiles room-a)) (level/lit-tiles lvl)))))

(deftest lit-tiles-includes-bounding-walls
  (let [lvl (level-with-rooms [(make-room [1 1 3 3] :lit? true) room-b])]
    (is (contains? (level/lit-tiles lvl) [0 0]) "corner wall of the lit room")
    (is (contains? (level/lit-tiles lvl) [2 0]) "top wall of the lit room")))
