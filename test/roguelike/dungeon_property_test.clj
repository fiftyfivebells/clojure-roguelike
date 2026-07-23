(ns roguelike.dungeon-property-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [roguelike.dungeon :as dungeon]))

(def gen-world-seed
  (gen/choose 0 0xFFFFFFFF))

(def gen-level-id
  (gen/choose 0 1000))

(defspec generate-produces-a-single-connected-component 500
  (prop/for-all [world-seed gen-world-seed
                 level-id gen-level-id]
    (let [lvl (dungeon/generate world-seed level-id)]
      (= 1 (count (#'dungeon/components lvl))))))
