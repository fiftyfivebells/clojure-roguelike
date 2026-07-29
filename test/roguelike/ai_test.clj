(ns roguelike.ai-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.ai :as ai]
            [roguelike.level :as level]
            [roguelike.pathfinding :as pathfinding]
            [roguelike.rng :as rng]
            [roguelike.world :as world]))

;;; Helpers

(defn- make-player
  [overrides]
  (merge {:entity/id 0 :entity/type :player :pos [10 10] :next-time 0} overrides))

(defn- make-monster
  [id overrides]
  (merge {:entity/id id :entity/type :generic-monster :pos [12 12] :next-time 0} overrides))

(defn- wall-off
  "Turns each of the given positions into a wall tile."
  [level positions]
  (reduce #(level/set-tile %1 %2 (level/wall)) level positions))

(defn- make-world
  "Builds a minimal world for ai tests: an 80x22 test-level bordered by walls, a player,
  and any monsters supplied. walls is a seq of positions to turn into wall tiles."
  [& {:keys [player monsters walls]
      :or   {player (make-player {}) monsters [] walls []}}]
  (let [current-level (-> (level/test-level)
                          (wall-off walls)
                          (as-> lvl (reduce level/add-entity lvl monsters)))]
    {:player           player
     :current-level-id 0
     :levels           {0 current-level}
     :next-tick        10
     :current-time     0
     :rng-state        (rng/make 42)
     :next-entity-id   (inc (apply max 0 (map :entity/id monsters)))}))

(defn- chebyshev
  "Step-distance between two cells on this 8-connected grid."
  [[ax ay] [bx by]]
  (max (abs (- ax bx)) (abs (- ay by))))

(def ^:private unit-directions
  #{[-1 -1] [0 -1] [1 -1]
    [-1  0]        [1  0]
    [-1  1] [0  1] [1  1]})

(defn- chased?
  "The chase branch returns the world untouched; wander consumes RNG. Comparing rng-state
  is the reliable signal, since a random wander delta can coincidentally look like a
  chase move."
  [world-in world-out]
  (= (:rng-state world-in) (:rng-state world-out)))

;;; chase

(deftest chase-steps-diagonally-toward-player
  (testing "unique nearest neighbor produces an exact diagonal step"
    (let [monster (make-monster 1 {:pos [12 12]})
          w (make-world :monsters [monster])
          [_ action] (#'ai/chase w monster)]
      (is (= {:action/type :world/move :delta [-1 -1]} action)))))

(deftest chase-does-not-consume-rng
  (testing "the chase branch returns the world unchanged"
    (let [monster (make-monster 1 {:pos [12 12]})
          w (make-world :monsters [monster])
          [w' action] (#'ai/chase w monster)]
      (is (chased? w w'))
      (is (= :world/move (:action/type action))))))

(deftest chase-steps-strictly-closer
  (testing "orthogonal approach closes the distance by one, whichever tied step is picked"
    (let [monster (make-monster 1 {:pos [13 10]})
          w (make-world :monsters [monster])
          player-pos (world/player-pos w)
          [w' action] (#'ai/chase w monster)
          new-pos (mapv + (:pos monster) (:delta action))]
      (is (chased? w w'))
      (is (= (dec (chebyshev (:pos monster) player-pos))
             (chebyshev new-pos player-pos))))))

(deftest chase-routes-around-a-wall
  (testing "follows BFS distance rather than a greedy line into the wall"
    (let [monster (make-monster 1 {:pos [12 10]})
          walls (for [y (range 6 15)] [11 y])
          w (make-world :monsters [monster] :walls walls)
          field (pathfinding/distance-map (partial world/walkable-at? w)
                                          (world/player-pos w))
          [w' action] (#'ai/chase w monster)
          new-pos (mapv + (:pos monster) (:delta action))]
      (is (chased? w w'))
      (is (not= [-1 0] (:delta action))
          "a naive step-toward would walk straight into the wall column")
      (is (world/walkable-at? w new-pos))
      ;; several steps tie for shortest here (the wall must be cleared vertically either
      ;; way), so pin the invariant rather than one particular delta
      (is (= (dec (field (:pos monster))) (field new-pos))))))

(deftest chase-onto-adjacent-player
  (testing "closes the last square onto the player, since there is no attack action yet"
    (let [monster (make-monster 1 {:pos [11 10]})
          w (make-world :monsters [monster])
          [w' action] (#'ai/chase w monster)]
      (is (chased? w w'))
      (is (= {:action/type :world/move :delta [-1 0]} action)))))

(deftest chase-emits-unit-steps
  (testing "every chase delta is a single step and never stands still"
    (doseq [pos [[12 12] [13 10] [11 10] [8 13]]]
      (let [monster (make-monster 1 {:pos pos})
            w (make-world :monsters [monster])
            [_ action] (#'ai/chase w monster)
            delta (:delta action)]
        (is (contains? unit-directions delta) (str "from " pos))))))

(deftest chase-wanders-when-player-unreachable
  (testing "a walled-in monster has no field entry and no field neighbors"
    (let [monster (make-monster 1 {:pos [15 15]})
          walls (for [dx [-1 0 1] dy [-1 0 1]
                      :when (not (and (zero? dx) (zero? dy)))]
                  [(+ 15 dx) (+ 15 dy)])
          w (make-world :monsters [monster] :walls walls)
          [w' action] (#'ai/chase w monster)]
      (is (not (chased? w w')))
      (is (contains? unit-directions (:delta action))))))

(deftest chase-wanders-when-no-step-improves
  (testing "standing on the player, every neighbor is further away"
    (let [monster (make-monster 1 {:pos [10 10]})
          w (make-world :monsters [monster])
          [w' action] (#'ai/chase w monster)]
      (is (not (chased? w w')))
      (is (contains? unit-directions (:delta action))))))
