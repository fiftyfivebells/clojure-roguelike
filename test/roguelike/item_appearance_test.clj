(ns roguelike.item-appearance-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.ui.appearance.item :as item]))

(def ^:private flavors
  {:potion/lesser-healing {:adjective :bubbling :color :azure}
   :potion/harm           {:adjective :glowing :color :crimson}
   :scroll/phase-door     {:title [[:ka :zor] [:en :mul]]}})

(defn- stack
  [kind n]
  {:item/kind kind :item/quantity n})

(deftest display-name-is-singular-with-no-markers
  (is (= "bubbling azure potion" (item/display-name (stack :potion/lesser-healing 3) flavors #{})))
  (is (= "Potion of Lesser Healing"
         (item/display-name (stack :potion/lesser-healing 3) flavors #{:potion/lesser-healing})))
  (is (= "dagger" (item/display-name (stack :edged/dagger 1) flavors #{}))))

(deftest describe-single-items-get-an-article
  (is (= "a bubbling azure potion" (item/describe (stack :potion/lesser-healing 1) flavors #{})))
  (is (= "a Potion of Harm" (item/describe (stack :potion/harm 1) flavors #{:potion/harm})))
  (is (= "a scroll titled KAZOR ENMUL" (item/describe (stack :scroll/phase-door 1) flavors #{})))
  (is (= "a Scroll of Phase Door" (item/describe (stack :scroll/phase-door 1) flavors #{:scroll/phase-door})))
  (is (= "a longsword" (item/describe (stack :edged/longsword 1) flavors #{}))))

(deftest describe-article-follows-the-finished-name
  (testing "the color is 'azure', but the adjective comes first, so it's 'a'"
    (is (= "a frothy azure potion"
           (item/describe (stack :potion/harm 1) {:potion/harm {:adjective :frothy :color :azure}} #{})))))

(deftest describe-stacks-get-a-count-and-plural
  (is (= "2 bubbling azure potions" (item/describe (stack :potion/lesser-healing 2) flavors #{})))
  (is (= "5 Potions of Harm" (item/describe (stack :potion/harm 5) flavors #{:potion/harm})))
  (is (= "3 scrolls titled KAZOR ENMUL" (item/describe (stack :scroll/phase-door 3) flavors #{})))
  (is (= "2 Scrolls of Phase Door" (item/describe (stack :scroll/phase-door 2) flavors #{:scroll/phase-door}))))

(deftest describe-an-empty-stack-says-no-more
  (is (= "no more Potions of Harm" (item/describe (stack :potion/harm 0) flavors #{:potion/harm})))
  (is (= "no more glowing crimson potions" (item/describe (stack :potion/harm 0) flavors #{}))))
