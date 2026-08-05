(ns roguelike.render-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.render :as render]))

;; camera-origin is private; #' reaches it. Drop the #' if you made it public.
(def camera-origin #'render/camera-origin)

(deftest camera-centers-on-player-in-open-space
  (testing "player away from all edges: camera centers, player lands mid-viewport"
    ;; level 100x50, viewport 80x24, player at [50 25]
    ;; raw = player - viewport/2 = [50-40, 25-12] = [10 13], both in range
    (is (= [10 13]
           (camera-origin [50 25] [80 24] [100 50])))))

(deftest camera-clamps-at-low-edges
  (testing "player near top-left: camera pins to [0 0], player draws near the corner"
    ;; raw = [5-40, 3-12] = [-35 -9], both below 0 -> clamp up to 0
    (is (= [0 0]
           (camera-origin [5 3] [80 24] [100 50])))))

(deftest camera-clamps-at-high-edges
  (testing "player near bottom-right: camera pins to max origin"
    ;; max origin = level - viewport = [100-80, 50-24] = [20 26]
    ;; raw = [95-40, 47-12] = [55 35], both above max -> clamp down
    (is (= [20 26]
           (camera-origin [95 47] [80 24] [100 50])))))

(deftest camera-pins-to-origin-when-level-smaller-than-viewport
  (testing "level fits entirely on screen: no scrolling, origin [0 0] regardless of player pos"
    ;; level 40x20, viewport 80x24 -> level <= viewport on both axes
    (is (= [0 0]
           (camera-origin [30 15] [80 24] [40 20])))
    ;; and it stays [0 0] even with the player at the level's far corner
    (is (= [0 0]
           (camera-origin [39 19] [80 24] [40 20])))))
