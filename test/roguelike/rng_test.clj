(ns roguelike.rng-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.rng :refer [next-rng-state]]))

(deftest test-sfc32-deterministic-sequence
  (testing "Sequence with initial state [0 0 0 1]"
    (let [initial-state [0 0 0 1]
          ;; Reference sequence from official sfc32 implementation
          ;; https://github.com/chrisgam/js-sfc32
          expected-outputs [1 2 12 18874399 56669315 2581679515 3653559774 1064146342 1496911795 4955899]]
      (loop [state initial-state
             outputs []
             expected expected-outputs]
        (if (empty? expected)
          (is (= outputs expected-outputs))
          (let [[new-state output] (next-rng-state state)]
            (is (= output (first expected))
                (str "Output mismatch at iteration " (count outputs)))
            (recur new-state (conj outputs output) (rest expected)))))))
  (testing "Sequence with initial state [1 2 3 4]"
    (let [initial-state [1 2 3 4]
          ;; Reference sequence from official sfc32 implementation
          expected-outputs [7 34 56623200 188882296 3431242869 399395954 785775158 3843710725 2124393435 4040705074]]
      (loop [state initial-state
             outputs []
             expected expected-outputs]
        (if (empty? expected)
          (is (= outputs expected-outputs))
          (let [[new-state output] (next-rng-state state)]
            (is (= output (first expected))
                (str "Output mismatch at iteration " (count outputs)))
            (recur new-state (conj outputs output) (rest expected))))))))

(deftest test-sfc32-state-progression
  (let [initial [0 0 0 1]
        [state1 _out1] (next-rng-state initial)
        [state2 _out2] (next-rng-state state1)
        [state3 _out3] (next-rng-state state2)]
    (testing "Counter increments each iteration"
      (is (= (nth state1 3) 2))
      (is (= (nth state2 3) 3))
      (is (= (nth state3 3) 4)))
    (testing "State components update"
      (is (not= state1 initial))
      (is (not= state2 state1))
      (is (not= state3 state2)))))

(deftest test-sfc32-output-is-32bit
  (let [initial [0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF]
        ;; Reference first 5 outputs from official implementation
        expected-first-outputs [4294967293 4286578679 4286578661 4154458059 3921867616]]
    (loop [state initial n 0]
      (if (< n 100)
        (let [[new-state output] (next-rng-state state)]
          (is (<= 0 output 0xFFFFFFFF)
              (str "Output out of range at iteration " n ": " (format "0x%x" output)))
          (when (< n 5)
            (is (= output (nth expected-first-outputs n))
                (str "Output mismatch at iteration " n)))
          (recur new-state (inc n)))
        nil))))

(deftest test-sfc32-state-components-in-range
  (let [initial [0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF]]
    (loop [state initial n 0]
      (if (< n 100)
        (let [[new-state output] (next-rng-state state)]
          (doseq [[idx component] (map-indexed vector new-state)]
            (is (<= 0 component 0xFFFFFFFF)
                (str "State component " idx " out of range at iteration " n
                     ": " (format "0x%x" component))))
          (recur new-state (inc n)))
        nil))))

;;; Mix Helpers

(defn- hamming-distance
  "Number of differing bits between two 32-bit values. A well-mixed avalanche
   function should decorrelate similar inputs to an average distance of ~16
   bits (half of 32) out of 32."
  [a b]
  (Long/bitCount (bit-xor a b)))

(def ^:private sample-size 2000)

;; sample-size draws of a well-mixed 32-bit avalanche should average close to
;; 16 bits of difference; this range gives slack for sampling noise while
;; still failing if outputs are correlated (e.g. off by a fixed small delta).
(defn- assert-well-mixed
  [avg]
  (is (< 13.0 avg 19.0) (str "average hamming distance was " avg ", expected ~16")))

;;; seed-to-initial-state

(deftest seed-to-initial-state-counter-always-starts-at-one
  (dotimes [i 20]
    (is (= 1 (nth (#'rng/seed-to-initial-state i) 3)))))

(deftest seed-to-initial-state-is-deterministic
  (is (= (#'rng/seed-to-initial-state 42) (#'rng/seed-to-initial-state 42))))

(deftest seed-to-initial-state-adjacent-seeds-are-decorrelated
  (testing "component a (fmix32 seed) for adjacent seeds"
    (let [distances (for [s (range sample-size)]
                      (hamming-distance (nth (#'rng/seed-to-initial-state s) 0)
                                        (nth (#'rng/seed-to-initial-state (inc s)) 0)))
          avg (/ (reduce + distances) (double sample-size))]
      (assert-well-mixed avg)))
  (testing "component b (fmix32 (seed xor C1)) for adjacent seeds"
    (let [distances (for [s (range sample-size)]
                      (hamming-distance (nth (#'rng/seed-to-initial-state s) 1)
                                        (nth (#'rng/seed-to-initial-state (inc s)) 1)))
          avg (/ (reduce + distances) (double sample-size))]
      (assert-well-mixed avg))))

;;; mix

(deftest mix-same-inputs-are-deterministic
  (is (= (rng/mix 123 :layout 5) (rng/mix 123 :layout 5))))

(deftest mix-returns-a-32-bit-value
  (dotimes [i 200]
    (is (<= 0 (rng/mix i :layout i) 0xFFFFFFFF))))

(deftest mix-adjacent-level-ids-are-decorrelated
  (let [seed 987654321
        distances (for [lid (range sample-size)]
                    (hamming-distance (rng/mix seed :layout lid)
                                      (rng/mix seed :layout (inc lid))))
        avg (/ (reduce + distances) (double sample-size))]
    (assert-well-mixed avg)))

(deftest mix-different-stream-keys-are-decorrelated
  (let [distances (for [lid (range sample-size)]
                    (hamming-distance (rng/mix 42 :layout lid)
                                      (rng/mix 42 :spawn lid)))
        avg (/ (reduce + distances) (double sample-size))]
    (assert-well-mixed avg)))

(deftest mix-different-world-seeds-are-decorrelated
  (let [distances (for [seed (range sample-size)]
                    (hamming-distance (rng/mix seed :layout 5)
                                      (rng/mix (inc seed) :layout 5)))
        avg (/ (reduce + distances) (double sample-size))]
    (assert-well-mixed avg)))
