(ns roguelike.rng-property-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [roguelike.rng :refer [next-rng-state]]))

(def gen-word
  (gen/choose 0 0xFFFFFFFF))

(def gen-state
  (gen/vector gen-word 4))

(defspec output-always-32-bit 500
  (prop/for-all [state gen-state]
                (let [[new-state _output] (next-rng-state state)]
                  (every? (fn [word] (<= 0 word 0xFFFFFFFF)) new-state))))

(defspec state-words-always-32-bit 500
  (prop/for-all [state gen-state]
    (let [[new-state _output] (next-rng-state state)]
      (every? (fn [word] (<= 0 word 0xFFFFFFFF)) new-state))))

(defspec counter-increments-by-one 500
  (prop/for-all [state gen-state]
    (let [[[_ _ _ d'] _] (next-rng-state state)
          d (nth state 3)]
      (= d' (bit-and 0xFFFFFFFF (inc d))))))
