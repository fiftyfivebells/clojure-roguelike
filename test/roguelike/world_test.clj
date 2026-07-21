(ns roguelike.world-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
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
    (is (seq (:known (:current-level observed))))
    (is (= observed deserialized))))
