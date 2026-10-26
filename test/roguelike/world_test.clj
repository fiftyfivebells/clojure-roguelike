(ns roguelike.world-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [roguelike.level :as level]
            [roguelike.world :as world]))

(defn- make-player
  [overrides]
  (merge {:entity/id 0 :entity/type :player :pos [1 1] :next-time 0
          :hp/current 10 :hp/max 10 :sight/radius 2}
         overrides))

(defn- make-monster
  [id overrides]
  (merge {:entity/id id :entity/type :monster :pos [2 2] :next-time 0 :sight/radius 2}
         overrides))

;;; A level with a single room, big enough that its far corner sits well
;;; outside a small torch radius:
;;;   ############
;;;   #..........#
;;;   #..........#
;;;   #..........#
;;;   #..........#
;;;   #..........#
;;;   ############
(def ^:private room-bounds [1 1 10 5])
(def ^:private room-near-corner [2 2])
(def ^:private room-far-corner [9 4])
(def ^:private room-far-wall [11 4]) ; right-hand wall, well past torch radius

(defn- make-lit-room-level
  "A single-room level (see room-bounds) with the given monsters added and
  :rooms set to one unlit room. The room only becomes :lit? via
  world/observe or level/light-room-at, not on construction."
  [& monsters]
  (let [floor-level (reduce (fn [lvl [x y]] (level/set-tile lvl [x y] (level/floor)))
                            (level/solid-level 12 7)
                            (for [x (range 1 11) y (range 1 6)] [x y]))
        with-room (level/set-rooms floor-level [{:bounds room-bounds :lit? false}])]
    (reduce level/add-entity with-room monsters)))

(defn- make-world
  [& {:keys [player level current-time next-tick]
      :or   {player (make-player {}) level (make-lit-room-level)
             current-time 0 next-tick 10}}]
  {:player           player
   :current-level-id 0
   :levels           {0 level}
   :next-tick        next-tick
   :current-time     current-time
   :next-entity-id   1})

(deftest world-round-trips-through-edn
  (let [walked (reduce (fn [w delta]
                          (first (world/update-world w (world/player-id w)
                                                      {:action/type :world/move :delta delta})))
                        (world/new-world)
                        [[1 0] [1 0] [0 1]])
        observed (world/observe walked)
        serialized (pr-str observed)
        deserialized (edn/read-string serialized)]
    (is (seq (:known (world/current-level observed))))
    (is (= observed deserialized))))

(deftest a-new-level-arrives-populated
  (let [w (world/new-world)]
    (is (seq (level/entities-of (world/current-level w))))
    (is (nil? (level/entity-at (world/current-level w) (world/player-pos w)))
        "the player never starts on top of a monster")))

(deftest descending-populates-the-new-level
  (let [[w _] (world/descend (world/new-world))]
    (is (seq (level/entities-of (world/current-level w))))
    (is (nil? (level/entity-at (world/current-level w) (world/player-pos w)))
        "the player never arrives on top of a monster")))

(deftest a-populated-level-keeps-its-spawn-rng
  (let [w (world/new-world)]
    (is (some? (level/rng-state (world/current-level w)))
        "later waves of monsters continue this stream instead of re-deriving it")))

(deftest each-level-gets-its-own-spawn-rng
  (let [w (world/new-world)
        [down _] (world/descend w)]
    (is (not= (level/rng-state (world/current-level w))
              (level/rng-state (world/current-level down))))))

(deftest revisiting-a-level-does-not-respawn-it
  (let [[down _] (world/descend (world/new-world))
        [up _] (world/ascend down)
        [again _] (world/descend up)]
    (is (= (level/entities-of (world/current-level down))
           (level/entities-of (world/current-level again))))
    (is (= (:next-entity-id down) (:next-entity-id again))
        "no fresh ids are handed out on a revisit")))

;;; player-dead?

(deftest player-dead-is-false-above-zero-hp
  (testing "a player with hp remaining is not dead"
    (is (false? (world/player-dead? {:player (make-player {:hp/current 1})})))))

(deftest player-dead-is-true-at-zero-hp
  (testing "hp/current 0 is the dead threshold"
    (is (true? (world/player-dead? {:player (make-player {:hp/current 0})})))))

(deftest player-dead-is-true-below-zero-hp
  (testing "negative hp/current (overkill damage) should also count as dead"
    (is (true? (world/player-dead? {:player (make-player {:hp/current -5})})))))

;;; Room lighting / lit-room visibility

(deftest observe-lights-the-players-room
  (let [w (make-world :player (make-player {:pos room-near-corner}))
        observed (world/observe w)]
    (is (:lit? (level/room-at (world/current-level observed) room-near-corner)))))

(deftest observe-reveals-the-whole-lit-room-beyond-torch-radius
  (testing "the far corner of the room is well outside the player's torch
            radius, but becomes visible once the room is lit"
    (let [w (make-world :player (make-player {:pos room-near-corner}))
          observed (world/observe w)]
      (is (contains? (level/known-tiles (world/current-level observed)) room-far-corner)))))

(deftest observe-reveals-the-lit-rooms-bounding-walls-too
  (testing "a wall bordering the lit room becomes visible, not just its floor"
    (let [w (make-world :player (make-player {:pos room-near-corner}))
          observed (world/observe w)]
      (is (contains? (level/known-tiles (world/current-level observed)) room-far-wall)))))

(deftest visible-cells-falls-back-to-torch-radius-when-nothing-is-lit
  (let [w (make-world :player (make-player {:pos room-near-corner}))]
    (is (not (contains? (world/visible-cells w (world/player-id w) 2) room-far-corner))
        "without any lit room, the far corner is outside torch radius 2")))

(deftest visible-cells-includes-lit-room-beyond-radius
  (let [w (make-world :player (make-player {:pos room-near-corner}))
        lit-level (update w :levels update 0 #(level/light-room-at % room-near-corner))]
    (is (contains? (world/visible-cells lit-level (world/player-id lit-level) 2) room-far-corner))))

(deftest visible-cells-benefits-any-actor-in-a-lit-room
  (testing "a monster standing in a lit room also sees beyond its own torch radius"
    (let [monster (make-monster 1 {:pos room-near-corner :sight/radius 2})
          w (make-world :level (make-lit-room-level monster))
          lit-level (update w :levels update 0 #(level/light-room-at % room-near-corner))]
      (is (contains? (world/visible-cells lit-level 1 2) room-far-corner)))))

;;; Items

(def ^:private potion {:item/kind :potion/lesser-healing :item/id 7})

(deftest known-item-checks-the-players-known-kinds
  (let [w (make-world :player (make-player {:knowledge {:item/known #{:potion/lesser-healing}}}))]
    (is (world/known-item? w :potion/lesser-healing))
    (is (not (world/known-item? w :potion/harm)))))

(deftest stepping-onto-a-known-item-reports-it-as-known
  (let [player (make-player {:pos [2 2] :knowledge {:item/known #{:potion/lesser-healing}}})
        lvl (level/add-item (make-lit-room-level) [3 2] (assoc potion :item/pos [3 2]))
        [_ events] (world/attempt-movement (make-world :player player :level lvl) 0 [1 0])
        seen (some #(when (= :world/items-seen (:event/type %)) %) events)]
    (is (= #{:potion/lesser-healing} (:known seen)))))

(deftest pickup-returns-a-vector-of-events
  (let [lvl (level/add-item (make-lit-room-level) [1 1] (assoc potion :item/pos [1 1]))
        [_ events] (world/update-world (make-world :player (make-player {:inventory []}) :level lvl)
                                       0 {:action/type :world/get})]
    (is (= [:world/pickup] (map :event/type events)))))

(deftest picking-up-the-same-kind-twice-makes-one-stack
  (let [stackable (assoc potion :item/stackable? true :item/quantity 1)
        lvl (-> (make-lit-room-level)
                (level/add-item [1 1] (assoc stackable :item/pos [1 1]))
                (level/add-item [2 1] (assoc stackable :item/id 8 :item/pos [2 1])))
        w (make-world :player (make-player {:inventory []}) :level lvl)
        [w _] (world/update-world w 0 {:action/type :world/get})
        w (world/move-actor w 0 [2 1])
        [w _] (world/update-world w 0 {:action/type :world/get})
        inv (:inventory (world/player-entity w))]
    (is (= 1 (count inv)))
    (is (= 2 (:item/quantity (first inv))))))

;;; Quaff

(defn- quaff-world
  [quantity known]
  (make-world :player (make-player {:inventory [{:item/kind :potion/harm :item/id 7
                                                 :item/stackable? true :item/quantity quantity}]
                                    :knowledge {:item/known known}})))

(deftest quaffing-uses-up-one-potion
  (let [[w _] (world/update-world (quaff-world 3 #{}) 0 {:action/type :world/quaff :item-id 7})]
    (is (= 2 (:item/quantity (first (world/player-inventory w)))))))

(deftest quaffing-the-last-potion-removes-the-stack
  (let [[w _] (world/update-world (quaff-world 1 #{}) 0 {:action/type :world/quaff :item-id 7})]
    (is (empty? (world/player-inventory w)))))

(deftest quaffing-identifies-the-kind
  (let [[w [event]] (world/update-world (quaff-world 2 #{}) 0 {:action/type :world/quaff :item-id 7})]
    (is (world/known-item? w :potion/harm))
    (is (= :world/quaffed (:event/type event)))
    (is (:learned? event))
    (is (= 2 (:item/quantity (:item event))) "the event shows the stack as it was before drinking")))

(deftest quaffing-a-known-potion-learns-nothing-new
  (let [[_ [event]] (world/update-world (quaff-world 2 #{:potion/harm}) 0 {:action/type :world/quaff :item-id 7})]
    (is (false? (:learned? event)))))
