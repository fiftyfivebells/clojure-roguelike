(ns roguelike.world-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [roguelike.level :as level]
            [roguelike.world :as world]))

(deftest world-round-trips-through-edn
  (let [walked (reduce (fn [w delta]
                          (first (world/update-world w (world/player-id w)
                                                      {:action/type :world/move :delta delta})))
                        (world/new-world)
                        [[1 0] [1 0] [0 1]])
        observed (world/observe walked)
        serialized (pr-str observed)
        deserialized (edn/read-string serialized)]
    (is (seq (:known (world/current-level observed))))
    (is (= observed deserialized))))

(deftest a-new-level-arrives-populated
  (let [w (world/new-world)]
    (is (seq (level/entities-of (world/current-level w))))
    (is (nil? (level/entity-at (world/current-level w) (world/player-pos w)))
        "the player never starts on top of a monster")))

(deftest descending-populates-the-new-level
  (let [[w _] (world/descend (world/new-world))]
    (is (seq (level/entities-of (world/current-level w))))
    (is (nil? (level/entity-at (world/current-level w) (world/player-pos w)))
        "the player never arrives on top of a monster")))

(deftest a-populated-level-keeps-its-spawn-rng
  (let [w (world/new-world)]
    (is (some? (level/rng-state (world/current-level w)))
        "later waves of monsters continue this stream instead of re-deriving it")))

(deftest each-level-gets-its-own-spawn-rng
  (let [w (world/new-world)
        [down _] (world/descend w)]
    (is (not= (level/rng-state (world/current-level w))
              (level/rng-state (world/current-level down))))))

(deftest revisiting-a-level-does-not-respawn-it
  (let [[down _] (world/descend (world/new-world))
        [up _] (world/ascend down)
        [again _] (world/descend up)]
    (is (= (level/entities-of (world/current-level down))
           (level/entities-of (world/current-level again))))
    (is (= (:next-entity-id down) (:next-entity-id again))
        "no fresh ids are handed out on a revisit")))
