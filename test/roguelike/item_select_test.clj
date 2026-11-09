(ns roguelike.item-select-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.inventory :as inventory]
            [roguelike.item :as item]
            [roguelike.ui.item-select :as item-select]))

(def ^:private flavors
  {:potion/lesser-healing {:adjective :bubbling :color :azure}
   :potion/harm           {:adjective :glowing :color :crimson}
   :scroll/phase-door     {:title [[:ka :zor] [:en :mul]]}})

(def ^:private inv
  [(item/instantiate :edged/dagger)
   (assoc (item/instantiate :potion/lesser-healing) :item/quantity 2)
   (item/instantiate :scroll/phase-door)])

(def ^:private pack (inventory/pack inv flavors #{}))

(deftest empty-filter-offers-everything
  (is (= [\a \b \c] (map :label (item-select/selectable pack {})))))

(deftest filter-keeps-the-full-pack-letters
  (testing "the scroll is b) in the inventory, and still b) when reading"
    (is (= [\b] (map :label (item-select/selectable pack {:item/verb :read})))))
  (is (= [\c] (map :label (item-select/selectable pack {:item/hands 1}))))
  (is (empty? (item-select/selectable pack {:item/verb :eat}))))

(deftest find-entry-looks-up-by-letter
  (let [entries (item-select/selectable pack {:item/verb :read})]
    (is (= :scroll/phase-door (:item/kind (:item (item-select/find-entry entries \b)))))
    (is (nil? (item-select/find-entry entries \a)) "a) is filtered out on this screen")))

(deftest rows-are-labelled-described-and-colored
  (is (= [{:label "a) " :text "2 bubbling azure potions"    :color :blue}
          {:label "b) " :text "a scroll titled KAZOR ENMUL" :color :white}
          {:label "c) " :text "a dagger"                    :color :white}]
         (item-select/rows (item-select/selectable pack {}) flavors #{}))))

(deftest header-shows-the-prompt-and-letter-range
  (is (= "Select Item: (Inventory: a-c, ESC)"
         (item-select/header "Select Item:" (item-select/selectable pack {}))))
  (is (= "Read which scroll? (Inventory: b-b, ESC)"
         (item-select/header "Read which scroll?" (item-select/selectable pack {:item/verb :read})))))
