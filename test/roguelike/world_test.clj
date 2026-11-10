(ns roguelike.world-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [roguelike.level :as level]
            [roguelike.rng :as rng]
            [roguelike.vision :as vision]
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

(deftest player-dead-at-zero-hp-or-below
  (let [dead? #(world/player-dead? {:player (make-player {:hp/current %})})]
    (is (false? (dead? 1)))
    (is (true? (dead? 0)))
    (is (true? (dead? -5)) "overkill damage counts too")))

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

(deftest visible-cells-is-empty-before-observe
  (is (empty? (world/visible-cells (make-world)))))

(deftest vision-falls-back-to-torch-radius-when-nothing-is-lit
  (let [w (make-world :player (make-player {:pos room-near-corner}))
        v (vision/compute (world/current-level w) room-near-corner 2)]
    (is (not (contains? (:visible v) room-far-corner))
        "without any lit room, the far corner is outside torch radius 2")))

(deftest visible-cells-includes-lit-room-beyond-radius-after-observe
  (let [w (world/observe (make-world :player (make-player {:pos room-near-corner})))]
    (is (contains? (world/visible-cells w) room-far-corner))))

(deftest monster-sees-a-player-standing-in-a-lit-room-beyond-its-radius
  (testing "the player's room is lit, so a monster in line of sight sees them
            even from outside its own torch radius"
    (let [w (world/observe (make-world :player (make-player {:pos room-near-corner})))]
      (is (vision/sees? (:vision w) room-far-corner 2)))))

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
    (is (= [:world/picked-up] (map :event/type events)))))

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
  ([quantity known] (quaff-world quantity known []))
  ([quantity known effects]
   (assoc (make-world :player (make-player {:inventory [{:item/kind :potion/harm :item/id 7
                                                         :item/stackable? true :item/quantity quantity
                                                         :item/effects effects}]
                                            :knowledge {:item/known known}}))
          :rng-state (rng/make 1))))

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

;;; Effects

(defn- effect-world
  [hp max-hp]
  (assoc (make-world :player (make-player {:hp/current hp :hp/max max-hp}))
         :rng-state (rng/make 1)))

(defn- player-hp
  [w]
  (:hp/current (world/player-entity w)))

(deftest heal-adds-the-rolled-amount
  (let [[w [event]] (world/apply-effect (effect-world 5 20) 0 {:effect/type :heal :dice 2 :sides 4})]
    (is (= :world/healed (:event/type event)))
    (is (<= 2 (:amount event) 8))
    (is (= (+ 5 (:amount event)) (player-hp w)))))

(deftest heal-stops-at-max-hp
  (let [[w _] (world/apply-effect (effect-world 9 10) 0 {:effect/type :heal :dice 2 :sides 4})]
    (is (= 10 (player-hp w)))))

(deftest damage-subtracts-the-rolled-amount
  (let [[w [event]] (world/apply-effect (effect-world 20 20) 0 {:effect/type :damage :dice 2 :sides 4})]
    (is (= :world/damaged (:event/type event)))
    (is (<= 2 (:amount event) 8))
    (is (= (- 20 (:amount event)) (player-hp w)))
    (is (not (world/player-dead? w)))))

(deftest fatal-damage-kills-the-player
  (let [[w events] (world/apply-effect (effect-world 1 20) 0 {:effect/type :damage :dice 2 :sides 4})]
    (is (world/player-dead? w))
    (is (= [:world/damaged :world/killed] (map :event/type events)))
    (is (= {:player? true} (:defender (second events))))))

(deftest effects-roll-from-the-world-rng
  (let [w (effect-world 5 20)
        [w' _] (world/apply-effect w 0 {:effect/type :heal :dice 2 :sides 4})]
    (is (not= (:rng-state w) (:rng-state w')))
    (is (= (world/apply-effect w 0 {:effect/type :heal :dice 2 :sides 4})
           (world/apply-effect w 0 {:effect/type :heal :dice 2 :sides 4}))
        "same rng state, same roll")))

(deftest quaffing-applies-the-potions-effects-after-the-quaffed-event
  (let [w (assoc-in (quaff-world 1 #{} [{:effect/type :damage :dice 1 :sides 1}]) [:player :hp/current] 10)
        [w events] (world/update-world w 0 {:action/type :world/quaff :item-id 7})]
    (is (= [:world/quaffed :world/damaged] (map :event/type events)))
    (is (= 9 (player-hp w)))))

(deftest quaffed-event-carries-the-stacks-letter
  (let [w (-> (quaff-world 2 #{})
              (update-in [:player :inventory] conj {:item/kind :edged/dagger :item/id 8 :item/quantity 1
                                                    :item/category :edged})
              (assoc-in [:player :inventory 0 :item/category] :potion))
        [_ [event]] (world/update-world w 0 {:action/type :world/quaff :item-id 7})]
    (is (= \a (:label event)))))

(deftest quaffed-event-keeps-the-letter-when-none-are-left
  (let [w (assoc-in (quaff-world 1 #{}) [:player :inventory 0 :item/category] :potion)
        [_ [event]] (world/update-world w 0 {:action/type :world/quaff :item-id 7})]
    (is (= \a (:label event)))))

;;; Teleport

(defn- teleport-world
  [& monsters]
  (assoc (make-world :level (apply make-lit-room-level monsters))
         :rng-state (rng/make 1)))

(deftest teleport-lands-at-the-rolled-distance
  ;; 1d1 always rolls 1, so the player moves exactly one step.
  (let [[w [event]] (world/apply-effect (teleport-world) 0 {:effect/type :teleport :dice 1 :sides 1})
        [x y] (world/player-pos w)]
    (is (= 1 (max (abs (- x 1)) (abs (- y 1)))))
    (is (= {:event/type :world/teleported :from [1 1] :to [x y]} event))))

(deftest teleport-never-lands-on-an-actor
  (testing "two of the three squares one step away are taken"
    (let [w (teleport-world (make-monster 1 {:pos [2 1]}) (make-monster 2 {:pos [2 2]}))
          [w _] (world/apply-effect w 0 {:effect/type :teleport :dice 1 :sides 1})]
      (is (= [1 2] (world/player-pos w))))))

(deftest teleport-goes-as-far-as-the-level-allows
  (testing "a 20-step roll in a room only 9 steps wide lands on the far wall side"
    (let [[w _] (world/apply-effect (teleport-world) 0 {:effect/type :teleport :dice 20 :sides 1})]
      (is (= 10 (first (world/player-pos w)))))))

;;; Wield / take off

(def ^:private dagger {:item/id 1 :item/kind :edged/dagger :item/quantity 1 :item/hands 1})
(def ^:private club   {:item/id 2 :item/kind :hafted/club :item/quantity 1 :item/hands 1})
(def ^:private spear  {:item/id 3 :item/kind :polearm/spear :item/quantity 1 :item/hands 2})

(defn- gear-world
  [inventory equipment]
  (make-world :player (make-player {:inventory inventory :equipment equipment
                                    :knowledge {:item/known #{}}})))

(defn- wield
  ([w item-id] (wield w item-id nil))
  ([w item-id slot]
   (world/update-world w 0 {:action/type :world/wield :item-id item-id :slot slot})))

(deftest wielding-moves-the-item-from-pack-to-equipment
  (let [[w [event]] (wield (gear-world [dagger] {}) 1)]
    (is (= [] (world/player-inventory w)))
    (is (= {:main-hand dagger} (world/player-equipment w)))
    (is (= :world/wielded (:event/type event)))
    (is (= :main-hand (:slot event)))
    (is (= [] (:removed event)))))

(deftest wielding-a-second-one-hander-fills-the-off-hand
  (let [[w _] (wield (gear-world [club] {:main-hand dagger}) 2)]
    (is (= {:main-hand dagger :off-hand club} (world/player-equipment w)))))

(deftest wielding-a-two-hander-returns-both-hands-to-the-pack
  (let [[w [event]] (wield (gear-world [spear] {:main-hand dagger :off-hand club}) 3)]
    (is (= {:main-hand spear} (world/player-equipment w)))
    (is (= #{1 2} (set (map :item/id (world/player-inventory w)))))
    (is (= [dagger club] (:removed event)))))

(deftest wielding-with-an-explicit-slot
  (let [w (gear-world [(assoc dagger :item/id 4)] {:main-hand dagger :off-hand club})
        [w [event]] (wield w 4 :off-hand)]
    (is (= :off-hand (:slot event)))
    (is (= 4 (get-in (world/player-equipment w) [:off-hand :item/id])))
    (is (= [club] (:removed event)))
    (is (= [club] (world/player-inventory w)))))

(deftest wielding-into-full-hands-needs-a-slot
  (is (thrown? clojure.lang.ExceptionInfo
               (wield (gear-world [(assoc dagger :item/id 4)] {:main-hand dagger :off-hand club}) 4))))

(deftest taking-off-returns-the-item-to-the-pack
  (let [[w [event]] (world/update-world (gear-world [] {:main-hand dagger}) 0
                                        {:action/type :world/take-off :slot :main-hand})]
    (is (= {} (world/player-equipment w)))
    (is (= [dagger] (world/player-inventory w)))
    (is (= {:event/type :world/took-off :item dagger :slot :main-hand}
           (dissoc event :known)))))
