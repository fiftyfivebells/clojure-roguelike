(ns roguelike.ui-test
  (:require [clojure.test :refer [deftest is]]
            [roguelike.ui :as ui]))

(def ^:private flavors {:potion/harm {:adjective :glowing :color :crimson}})

(def ^:private harm {:item/kind :potion/harm :item/quantity 3})

(defn- message
  [event]
  (:current-msg (ui/apply-event (ui/new-ui) event flavors)))

(deftest quaffed-says-whats-left-by-its-real-name
  (is (= "You have 2 Potions of Harm (b)."
         (message {:event/type :world/quaffed :item harm :learned? true :label \b}))))

(deftest quaffing-the-last-one-says-no-more
  (is (= "You have no more Potions of Harm (a)."
         (message {:event/type :world/quaffed :item (assoc harm :item/quantity 1) :label \a}))))

(deftest healing-messages-follow-angbands-thresholds
  (is (= ["You feel a little better." "You feel better." "You feel much better." "You feel very good."]
         (map #(message {:event/type :world/healed :amount %}) [4 5 15 35]))))

(deftest damage-message
  (is (= "You feel very sick." (message {:event/type :world/damaged :amount 3}))))

(def ^:private spear {:item/kind :polearm/spear :item/quantity 1 :item/hands 2})
(def ^:private club {:item/kind :hafted/club :item/quantity 1 :item/hands 1})
(def ^:private robe {:item/kind :soft-armor/robe :item/quantity 1 :item/slot :body})

(deftest wielded-message-names-the-hand
  (is (= "You are wielding a club (off hand)."
         (message {:event/type :world/wielded :item club :slot :off-hand :removed [] :known #{}}))))

(deftest wielded-message-says-what-came-off-first
  (is (= "You were wielding a club. You were wielding a club. You are wielding a spear (main hand)."
         (message {:event/type :world/wielded :item spear :slot :main-hand
                   :removed [club club] :known #{}}))))

(deftest armor-is-worn-not-wielded
  (is (= "You are wearing a robe."
         (message {:event/type :world/wielded :item robe :slot :body :removed [] :known #{}})))
  (is (= "You were wearing a robe."
         (message {:event/type :world/took-off :item robe :slot :body :known #{}}))))
