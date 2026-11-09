(ns roguelike.inventory-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.inventory :as inventory]
            [roguelike.item :as item]))

(defn- make-item
  [kind id]
  (assoc (item/instantiate kind) :item/id id))

(deftest new-items-have-a-quantity-of-one
  (is (= 1 (:item/quantity (item/instantiate :potion/harm)))))

(deftest adding-to-an-empty-inventory
  (let [potion (make-item :potion/harm 1)]
    (is (= [potion] (inventory/add-item [] potion)))))

(deftest stackable-items-of-the-same-kind-merge
  (let [inv (inventory/add-items [] [(make-item :potion/harm 1)
                                     (make-item :potion/harm 2)
                                     (make-item :potion/harm 3)])]
    (is (= 1 (count inv)))
    (is (= 3 (:item/quantity (first inv))))
    (is (= 1 (:item/id (first inv))) "the existing stack keeps its identity")))

(deftest different-kinds-do-not-merge
  (let [inv (inventory/add-items [] [(make-item :potion/harm 1)
                                     (make-item :potion/lesser-healing 2)])]
    (is (= [1 1] (map :item/quantity inv)))))

(deftest non-stackable-items-never-merge
  (testing "two daggers stay as two entries"
    (let [inv (inventory/add-items [] [(make-item :edged/dagger 1)
                                       (make-item :edged/dagger 2)])]
      (is (= [1 2] (map :item/id inv)))
      (is (= [1 1] (map :item/quantity inv))))))

(deftest merging-adds-the-incoming-quantity
  (let [stack (assoc (make-item :scroll/phase-door 1) :item/quantity 4)
        more  (assoc (make-item :scroll/phase-door 2) :item/quantity 3)]
    (is (= 7 (:item/quantity (first (inventory/add-item [stack] more)))))))

;;; pack

(def ^:private flavors
  {:potion/lesser-healing {:adjective :bubbling :color :azure}
   :potion/harm           {:adjective :glowing :color :crimson}
   :scroll/phase-door     {:title [[:ka :zor]]}})

(defn- kinds-in-pack
  [inventory known]
  (map (comp :item/kind :item) (inventory/pack inventory flavors known)))

(deftest pack-sorts-by-category-like-angband
  (let [inv [(make-item :hafted/club 1)
             (make-item :edged/dagger 2)
             (make-item :scroll/phase-door 3)
             (make-item :polearm/spear 4)
             (make-item :potion/harm 5)]]
    (is (= [:potion/harm :scroll/phase-door :edged/dagger :polearm/spear :hafted/club]
           (kinds-in-pack inv #{})))))

(deftest unidentified-items-sort-after-known-ones-in-their-category
  (let [inv [(make-item :potion/harm 1)
             (make-item :potion/lesser-healing 2)]]
    (testing "both unknown: sorted by kind"
      (is (= [:potion/harm :potion/lesser-healing] (kinds-in-pack inv #{}))))
    (testing "knowing lesser-healing moves it ahead of the unknown potion"
      (is (= [:potion/lesser-healing :potion/harm]
             (kinds-in-pack inv #{:potion/lesser-healing}))))
    (testing "unidentified potions still come before scrolls"
      (is (= [:potion/harm :scroll/phase-door]
             (kinds-in-pack [(make-item :scroll/phase-door 3) (make-item :potion/harm 1)]
                            #{:scroll/phase-door}))))))

(deftest unflavored-items-count-as-known
  (testing "weapons have no flavor, so an empty known set doesn't push them back"
    (is (= [:edged/dagger :edged/longsword]
           (kinds-in-pack [(make-item :edged/longsword 1) (make-item :edged/dagger 2)] #{})))))

(deftest identical-unstackable-items-keep-pickup-order
  (is (= [1 2] (map (comp :item/id :item)
                    (inventory/pack [(make-item :edged/dagger 1) (make-item :edged/dagger 2)]
                                    flavors #{})))))

(deftest pack-labels-are-letters-in-display-order
  (let [inv [(make-item :edged/dagger 1) (make-item :potion/harm 2)]]
    (is (= [[\a :potion/harm] [\b :edged/dagger]]
           (map (juxt :label (comp :item/kind :item)) (inventory/pack inv flavors #{}))))
    (testing "letters shift when an item sorts in ahead"
      (is (= [\a \b \c]
             (map :label (inventory/pack (conj inv (make-item :scroll/phase-door 3)) flavors #{}))))
      (is (= :edged/dagger
             (:item/kind (:item (nth (inventory/pack (conj inv (make-item :scroll/phase-door 3))
                                                     flavors #{})
                                     2))))))))

(deftest empty-inventory-makes-an-empty-pack
  (is (empty? (inventory/pack [] flavors #{}))))

;;; find-item / remove-one

(deftest find-item-looks-up-by-id
  (let [inv [(make-item :potion/harm 1) (make-item :edged/dagger 2)]]
    (is (= :edged/dagger (:item/kind (inventory/find-item-by-id inv 2))))
    (is (nil? (inventory/find-item-by-id inv 99)))))

(deftest remove-one-decrements-a-stack
  (let [inv [(assoc (make-item :potion/harm 1) :item/quantity 3) (make-item :edged/dagger 2)]]
    (is (= [2 1] (map :item/quantity (inventory/remove-one inv 1))))))

(deftest remove-one-drops-the-last-of-a-stack
  (let [inv [(make-item :potion/harm 1) (make-item :edged/dagger 2)]]
    (is (= [2] (map :item/id (inventory/remove-one inv 1))))))

(deftest remove-quantity-takes-part-of-a-stack
  (let [inv [{:item/id 1 :item/quantity 5} {:item/id 2 :item/quantity 1}]]
    (is (= [3 1] (map :item/quantity (inventory/remove-quantity inv 1 2))))))

(deftest remove-quantity-of-the-whole-stack-removes-it
  (let [inv [{:item/id 1 :item/quantity 5} {:item/id 2 :item/quantity 1}]]
    (is (= [2] (map :item/id (inventory/remove-quantity inv 1 5))))))
