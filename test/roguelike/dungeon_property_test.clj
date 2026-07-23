(ns roguelike.dungeon-property-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [roguelike.dungeon :as dungeon]
            [roguelike.level :as level]))

(def gen-world-seed
  (gen/choose 0 0xFFFFFFFF))

(def gen-level-id
  (gen/choose 0 1000))

(defspec generate-produces-a-single-connected-component 500
  (prop/for-all [world-seed gen-world-seed
                 level-id gen-level-id]
    (let [lvl (dungeon/generate world-seed level-id)]
      (= 1 (count (#'dungeon/components lvl))))))

;;; Boundary tests

(defn- room-in-bounds?
  [[w h] {:keys [bounds]}]
  (let [[rx ry rw rh] bounds]
    (and (>= rx 0) (>= ry 0)
         (<= (+ rx rw) w) (<= (+ ry rh) h))))

(defspec rooms-always-stay-within-level-bounds 300
  (prop/for-all [world-seed gen-world-seed
                 level-id gen-level-id]
    (let [lvl (dungeon/generate world-seed level-id)
          dims (level/dimensions lvl)]
      (every? #(room-in-bounds? dims %) (:rooms lvl)))))

(defn- distinct-pairs
  [coll]
  (let [n (count coll)]
    (for [i (range n)
          j (range (inc i) n)]
      [(nth coll i) (nth coll j)])))

(defspec rooms-never-overlap 300
  (prop/for-all [world-seed gen-world-seed
                 level-id gen-level-id]
    (let [lvl (dungeon/generate world-seed level-id)
          rooms (:rooms lvl)]
      (every? (fn [[a b]] (not (#'dungeon/rooms-overlap? a b)))
              (distinct-pairs rooms)))))

(defn- border-coords
  [w h]
  (concat (for [x (range w)] [x 0])
          (for [x (range w)] [x (dec h)])
          (for [y (range h)] [0 y])
          (for [y (range h)] [(dec w) y])))

(defspec level-border-always-stays-wall 300
  (prop/for-all [world-seed gen-world-seed
                 level-id gen-level-id]
    (let [lvl (dungeon/generate world-seed level-id)
          [w h] (level/dimensions lvl)]
      (every? #(= (level/wall) (level/tile-at lvl %)) (border-coords w h)))))
