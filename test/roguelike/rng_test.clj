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
