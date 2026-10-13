(ns roguelike.flavor-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.item.flavor :as flavor]
            [roguelike.rng :as rng]))

(def kinds [:scroll/a :scroll/b :scroll/c :scroll/d :scroll/e :scroll/f])

(deftest make-title-test
  (testing "titles are quoted and advance the rng"
    (let [holder {:rng-state (rng/make 1)}
          [next-holder title] (flavor/make-title holder)]
      (is (re-matches #"\"[a-z]+( [a-z]+)?\"" title))
      (is (not= (:rng-state holder) (:rng-state next-holder))))))

(deftest roll-scroll-flavors-test
  (testing "every kind gets a title, and the titles are all different"
    (let [flavors (flavor/roll-scroll-flavors 42 kinds)
          titles (map :title (vals flavors))]
      (is (= (set kinds) (set (keys flavors))))
      (is (= (count kinds) (count (set titles))))))
  (testing "same seed gives same titles, different seed gives different ones"
    (is (= (flavor/roll-scroll-flavors 42 kinds)
           (flavor/roll-scroll-flavors 42 kinds)))
    (is (not= (flavor/roll-scroll-flavors 42 kinds)
              (flavor/roll-scroll-flavors 43 kinds)))))
