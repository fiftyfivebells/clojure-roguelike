(ns roguelike.dungeon-test
  (:require [clojure.test :refer [deftest is]]
            [roguelike.dungeon :as dungeon]
            [roguelike.level :as level]
            [roguelike.rng :as rng]))

;;; Helpers

(defn- stamp-floor-block
  "Fills a w x h rectangle of floor tiles into level, anchored at [x0 y0]."
  [level [x0 y0] [w h]]
  (reduce (fn [lvl [x y]] (level/set-tile lvl [x y] (level/floor)))
          level
          (for [x (range x0 (+ x0 w))
                y (range y0 (+ y0 h))]
            [x y])))

;;; ensure-connected

(deftest ensure-connected-joins-two-disconnected-components
  (let [lvl (-> (level/solid-level 20 10)
                (stamp-floor-block [1 1] [3 3])
                (stamp-floor-block [15 6] [3 3]))
        ctx {:level lvl :rng-state (rng/make 42)}
        before (#'dungeon/components lvl)
        result (#'dungeon/ensure-connected ctx)]
    (is (= 2 (count before)))
    (is (= 1 (count (#'dungeon/components (:level result)))))))

(deftest ensure-connected-joins-three-disconnected-components
  (let [lvl (-> (level/solid-level 30 10)
                (stamp-floor-block [1 1] [2 2])
                (stamp-floor-block [15 1] [2 2])
                (stamp-floor-block [27 7] [2 2]))
        ctx {:level lvl :rng-state (rng/make 7)}
        before (#'dungeon/components lvl)
        result (#'dungeon/ensure-connected ctx)]
    (is (= 3 (count before)))
    (is (= 1 (count (#'dungeon/components (:level result)))))))

(deftest ensure-connected-is-noop-when-already-connected
  (let [lvl (-> (level/solid-level 10 10)
                (stamp-floor-block [1 1] [5 5]))
        ctx {:level lvl :rng-state (rng/make 42)}]
    (is (= ctx (#'dungeon/ensure-connected ctx)))))

(deftest ensure-connected-is-noop-with-no-floor-tiles
  (let [lvl (level/solid-level 10 10)
        ctx {:level lvl :rng-state (rng/make 1)}]
    (is (= ctx (#'dungeon/ensure-connected ctx)))))

;;; generate reproducibility & rng-state exclusion

(deftest generate-is-reproducible-for-same-seed-and-level-id
  (is (= (dungeon/generate 12345 7) (dungeon/generate 12345 7))))

(deftest generate-does-not-store-rng-state-on-the-level
  (is (not (contains? (dungeon/generate 12345 7) :rng-state))))
