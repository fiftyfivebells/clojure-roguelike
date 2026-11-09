(ns roguelike.flavor-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.item.flavor :as flavor]))

(def ^:private scrolls [:scroll/a :scroll/b :scroll/c :scroll/d :scroll/e :scroll/f])
(def ^:private potions [:potion/a :potion/b :potion/c])
(def ^:private kinds {:scroll scrolls :potion potions})

(deftest scroll-titles
  (let [flavors (flavor/roll-flavors 42 kinds)
        titles (map (comp :title flavors) scrolls)]
    (testing "every scroll kind gets a title"
      (is (every? some? titles)))
    (testing "titles are 2-3 words of 2-3 syllable keywords"
      (doseq [title titles]
        (is (<= 2 (count title) 3))
        (is (every? #(and (<= 2 (count %) 3) (every? keyword? %)) title))))
    (testing "titles are all different"
      (is (= (count scrolls) (count (set titles)))))))

(deftest potion-flavors
  (let [flavors (flavor/roll-flavors 42 kinds)
        combos (map flavors potions)]
    (testing "every potion kind gets an adjective and a color"
      (is (every? #(and (:adjective %) (:color %)) combos)))
    (testing "combos are all different"
      (is (= (count potions) (count (set combos)))))))

(deftest flavors-come-from-the-seed
  (is (= (flavor/roll-flavors 42 kinds) (flavor/roll-flavors 42 kinds)))
  (is (not= (flavor/roll-flavors 42 kinds) (flavor/roll-flavors 43 kinds))))
