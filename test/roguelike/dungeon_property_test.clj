(ns roguelike.dungeon-property-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [roguelike.dungeon :as dungeon]
            [roguelike.level :as level]))

;; Generating a level is the slow part (~200ms), so each trial generates one
;; level and checks every property on it.

(def gen-world-seed
  (gen/choose 0 0xFFFFFFFF))

(def gen-level-id
  (gen/choose 0 1000))

(defn- connected?
  [lvl]
  (= 1 (count (#'dungeon/components lvl))))

(defn- rooms-in-bounds?
  [lvl]
  (let [[w h] (level/dimensions lvl)]
    (every? (fn [{[rx ry rw rh] :bounds}]
              (and (>= rx 0) (>= ry 0) (<= (+ rx rw) w) (<= (+ ry rh) h)))
            (:rooms lvl))))

(defn- rooms-never-overlap?
  [lvl]
  (let [rooms (vec (:rooms lvl))]
    (every? (fn [[i j]] (not (#'dungeon/rooms-overlap? (rooms i) (rooms j))))
            (for [i (range (count rooms)) j (range (inc i) (count rooms))] [i j]))))

(defn- border-is-wall?
  [lvl]
  (let [[w h] (level/dimensions lvl)
        border (concat (for [x (range w)] [x 0])
                       (for [x (range w)] [x (dec h)])
                       (for [y (range h)] [0 y])
                       (for [y (range h)] [(dec w) y]))]
    (every? #(= (level/wall) (level/tile-at lvl %)) border)))

(defspec generated-levels-are-well-formed 30
  (prop/for-all [world-seed gen-world-seed
                 level-id gen-level-id]
                (let [lvl (dungeon/generate world-seed level-id)]
                  (and (connected? lvl)
                       (rooms-in-bounds? lvl)
                       (rooms-never-overlap? lvl)
                       (border-is-wall? lvl)))))
