(ns roguelike.equipment-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.equipment :as equipment]
            [roguelike.item]))

(def dagger {:item/id 1 :item/kind :edged/dagger :item/hands 1
             :item/method :thrust :item/damage {:dice 1 :sides 4}})
(def club   {:item/id 2 :item/kind :hafted/club :item/hands 1
             :item/method :smash :item/damage {:dice 1 :sides 5}})
(def spear  {:item/id 3 :item/kind :polearm/spear :item/hands 2
             :item/method :thrust :item/damage {:dice 1 :sides 8}})
(def armor  {:item/id 4 :item/kind :soft-armor/leather :item/slot :body
             :item/armor-class 8})

(deftest slot-for-one-handed-weapons
  (testing "fills the main hand first"
    (is (= :main-hand (equipment/slot-for {} dagger))))
  (testing "then the off hand"
    (is (= :off-hand (equipment/slot-for {:main-hand club} dagger))))
  (testing "main hand when only the off hand is full"
    (is (= :main-hand (equipment/slot-for {:off-hand club} dagger))))
  (testing "ambiguous when both hands are full"
    (is (= :ambiguous (equipment/slot-for {:main-hand club :off-hand club} dagger))))
  (testing "replaces a two-handed weapon in the main hand"
    (is (= :main-hand (equipment/slot-for {:main-hand spear} dagger)))))

(deftest slot-for-other-items
  (is (= :main-hand (equipment/slot-for {:main-hand club :off-hand dagger} spear)))
  (is (= :body (equipment/slot-for {} armor))))

(deftest equip-into-empty-slot
  (is (= [{:main-hand dagger} []] (equipment/equip {} dagger :main-hand))))

(deftest equip-replaces-the-slot
  (is (= [{:main-hand dagger :off-hand club} [club]]
         (equipment/equip {:main-hand club :off-hand club} dagger :main-hand))))

(deftest two-handed-weapon-clears-both-hands
  (let [[eq displaced] (equipment/equip {:main-hand club :off-hand dagger :body armor}
                                        spear :main-hand)]
    (is (= {:main-hand spear :body armor} eq))
    (is (= [club dagger] displaced))))

(deftest off-hand-item-displaces-two-handed-weapon
  (is (= [{:off-hand dagger} [spear]]
         (equipment/equip {:main-hand spear} dagger :off-hand))))

(deftest unequip-returns-the-item
  (is (= [{} dagger] (equipment/unequip {:main-hand dagger} :main-hand)))
  (is (= [{} nil] (equipment/unequip {} :main-hand))))

(deftest weapon-attacks
  (testing "no weapons, no attacks"
    (is (= [] (equipment/weapon-attacks {:body armor}))))
  (testing "one attack per weapon, main hand first"
    (let [[main off] (equipment/weapon-attacks {:main-hand dagger :off-hand club})]
      (is (= {:method :thrust :effect :physical :dice 1 :sides 4} main))
      (is (= :smash (:method off)))
      (is (neg? (:to-hit-mod off))))))

(deftest armor-class-sums-equipment
  (is (= 0 (equipment/armor-class {})))
  (is (= 8 (equipment/armor-class {:main-hand dagger :body armor}))))

(deftest catalog-items-go-to-the-right-slots
  (let [item #(roguelike.item/instantiate %)]
    (is (= :main-hand (equipment/slot-for {} (item :edged/dagger))))
    (is (= :off-hand (equipment/slot-for {:main-hand dagger} (item :edged/dagger))))
    (is (= :main-hand (equipment/slot-for {:main-hand dagger} (item :polearm/spear))))
    (is (= :body (equipment/slot-for {} (item :soft-armor/soft-leather-armor))))))
