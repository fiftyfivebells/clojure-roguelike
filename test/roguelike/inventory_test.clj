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
