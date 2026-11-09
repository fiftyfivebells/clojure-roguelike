(ns roguelike.game-test
  (:require [clojure.test :refer [deftest is]]
            [roguelike.game :as game]
            [roguelike.input :as input]
            [roguelike.item :as item]
            [roguelike.world :as world])
  (:import [com.googlecode.lanterna.input KeyType]))

;; generating a game builds a whole dungeon (~200ms), and games are values,
;; so every test starts from this one
(def ^:private fresh-game (game/new-game))

(def ^:private dagger (assoc (item/instantiate :edged/dagger) :item/id 50))
(def ^:private potion (assoc (item/instantiate :potion/harm) :item/id 51))

(defn- with-inventory
  [g items]
  (assoc-in g [:world :player :inventory] items))

(defn- press
  [g key-event]
  (game/player-action g (input/key->action key-event (game/current-mode g))))

(def ^:private open-inventory (input/key->action {:key \i} nil))

(deftest i-opens-the-inventory-picker
  (let [g (game/player-action (with-inventory fresh-game [dagger]) open-inventory)]
    (is (= {:screen :item-select
            :prompt "Select Item:"
            :filter {}
            :empty "You have nothing in your inventory."}
           (game/current-mode g)))))

(deftest empty-inventory-just-shows-the-empty-message
  (let [g (game/player-action fresh-game open-inventory)]
    (is (nil? (game/current-mode g)))
    (is (= "You have nothing in your inventory." (get-in g [:ui :current-msg])))))

(deftest picker-with-nothing-matching-its-filter-shows-the-empty-message
  (let [g (game/player-action (with-inventory fresh-game [dagger])
                              {:action/type :ui/item-select :prompt "Quaff which potion?"
                               :filter {:item/verb :quaff} :empty "You have no potions to quaff."})]
    (is (nil? (game/current-mode g)))
    (is (= "You have no potions to quaff." (get-in g [:ui :current-msg])))))

(deftest escape-closes-the-picker
  (let [g (game/player-action (with-inventory fresh-game [dagger]) open-inventory)]
    (is (nil? (game/current-mode (press g {:key nil :key-type KeyType/Escape}))))))

(deftest opening-the-picker-takes-no-game-time
  (let [g (with-inventory fresh-game [dagger])]
    (is (= (:world g) (:world (game/player-action g open-inventory))))))

(def ^:private quaff-picker
  {:action/type :ui/item-select :prompt "Quaff which potion?" :filter {:item/verb :quaff}
   :empty "You have no potions to quaff." :on-select {:action/type :world/quaff}})

(def ^:private resolve-selection #'game/resolve-selection)

(deftest picking-closes-the-picker-and-becomes-on-select-with-the-item-id
  ;; quaff doesn't exist yet, so check what the pick turns into instead of running it.
  (let [g (game/player-action (with-inventory fresh-game [dagger potion]) quaff-picker)
        [picked action] (resolve-selection g {:action/type :ui/select :label \a})]
    (is (= {:action/type :world/quaff :item-id 51} action))
    (is (nil? (game/current-mode picked)) "the picker closed")))

(deftest non-select-actions-pass-through-resolution
  (let [g fresh-game
        action {:action/type :world/wait}]
    (is (= [g action] (resolve-selection g action)))))

(deftest letters-not-on-screen-do-nothing
  (let [g (game/player-action (with-inventory fresh-game [dagger potion]) quaff-picker)]
    (is (= g (press g {:key \b :key-type KeyType/Character})) "b) is the dagger, filtered out")
    (is (= g (press g {:key \z :key-type KeyType/Character})) "no z) at all")))

(deftest inventory-letters-do-nothing-until-the-item-menu-exists
  (let [g (game/player-action (with-inventory fresh-game [dagger]) open-inventory)]
    (is (= g (press g {:key \a :key-type KeyType/Character})))))

(deftest q-then-a-letter-quaffs-and-takes-a-turn
  (let [g (with-inventory fresh-game [dagger (assoc potion :item/quantity 2)])
        drank (-> g
                  (press {:key \q :key-type KeyType/Character})
                  (press {:key \a :key-type KeyType/Character}))]
    (is (nil? (game/current-mode drank)) "the picker closed")
    (is (= [1] (map :item/quantity (filter #(= 51 (:item/id %))
                                           (get-in drank [:world :player :inventory])))))
    (is (contains? (get-in drank [:world :player :knowledge :item/known]) :potion/harm))
    (is (= 10 (- (get-in drank [:world :player :next-time])
                 (get-in g [:world :player :next-time])))
        "quaffing costs a turn")))

(deftest drinking-harm-at-low-hp-ends-the-game
  (let [g (-> fresh-game
              (with-inventory [potion])
              (assoc-in [:world :player :hp/current] 1))
        drank (-> g
                  (press {:key \q :key-type KeyType/Character})
                  (press {:key \a :key-type KeyType/Character}))]
    (is (= :dead (:screen (game/current-mode drank))))
    (is (= "You have died. Press any key to continue." (get-in drank [:ui :current-msg])))))

(deftest drinking-records-both-messages-in-order
  (let [g (with-inventory fresh-game [(assoc potion :item/quantity 2)])
        drank (-> g
                  (press {:key \q :key-type KeyType/Character})
                  (press {:key \a :key-type KeyType/Character}))]
    (is (= ["You have a Potion of Harm (a)." "You feel very sick."]
           (take-last 2 (get-in drank [:ui :messages]))))
    (is (= "You feel very sick." (get-in drank [:ui :current-msg]))
        "only the last message is on screen")))

(deftest q-and-r-open-their-pickers
  (is (= {:action/type :ui/item-select
          :prompt "Quaff which potion?"
          :filter {:item/verb :quaff}
          :empty "You have no potions to quaff."
          :on-select {:action/type :world/quaff}}
         (input/key->action {:key \q} nil)))
  (is (= {:action/type :ui/item-select
          :prompt "Read which scroll?"
          :filter {:item/verb :read}
          :empty "You have no scrolls to read."
          :on-select {:action/type :world/read}}
         (input/key->action {:key \r} nil))))

(deftest reading-phase-door-moves-the-player-and-uses-the-scroll
  (let [scroll (assoc (item/instantiate :scroll/phase-door) :item/id 52)
        g (with-inventory fresh-game [scroll])
        read (-> g
                 (press {:key \r :key-type KeyType/Character})
                 (press {:key \a :key-type KeyType/Character}))]
    (is (not= (get-in g [:world :player :pos]) (get-in read [:world :player :pos])))
    (is (empty? (get-in read [:world :player :inventory])))
    (is (= "You have no more Scrolls of Phase Door (a)." (get-in read [:ui :current-msg])))))

;;; drop

(defn- type-keys
  [g & keys]
  (reduce (fn [g k]
            (if (= k :enter)
              (press g {:key-type KeyType/Enter})
              (press g {:key k :key-type KeyType/Character})))
          g keys))

(deftest dropping-from-a-stack-asks-how-many
  (let [g (type-keys (with-inventory fresh-game [(assoc potion :item/quantity 3)]) \d \a)]
    (is (= {:screen    :number-select
            :prompt    "Drop how many?"
            :max       3
            :text      ""
            :on-select {:action/type :world/drop :item-id 51}}
           (game/current-mode g)))))

(defn- dropped-items
  "Floor items in g that weren't on the floor in before. The level spawns its
  own items, so tests can't pick out what was dropped by kind or id alone."
  [before g]
  (let [floor-before (set (world/current-level-items (:world before)))]
    (remove floor-before (world/current-level-items (:world g)))))

(deftest dropping-part-of-a-stack-splits-it
  (let [start (with-inventory fresh-game [(assoc potion :item/quantity 3)])
        g (type-keys start \d \a \2 :enter)
        floor (dropped-items start g)]
    (is (nil? (game/current-mode g)) "both pickers closed")
    (is (= [{:item/id 51 :item/quantity 1}]
           (map #(select-keys % [:item/id :item/quantity]) (get-in g [:world :player :inventory]))))
    (is (= [2] (map :item/quantity floor)))
    (is (not= 51 (:item/id (first floor))) "the split-off part gets its own id")
    (is (= (get-in g [:world :player :pos]) (:item/pos (first floor))))))

(deftest dropping-a-single-item-skips-the-quantity-prompt
  (let [start (with-inventory fresh-game [dagger])
        g (type-keys start \d \a)
        floor (dropped-items start g)]
    (is (nil? (game/current-mode g)))
    (is (empty? (get-in g [:world :player :inventory])))
    (is (= 1 (count floor)))))

;;; Wield / take off / equipment

(def ^:private club (assoc (item/instantiate :hafted/club) :item/id 52))
(def ^:private dagger-2 (assoc (item/instantiate :edged/dagger) :item/id 53))

(defn- with-equipment
  [g equipment]
  (assoc-in g [:world :player :equipment] equipment))

(defn- type-key
  [g k]
  (press g {:key k :key-type KeyType/Character}))

(deftest w-then-a-letter-wields-and-takes-a-turn
  (let [g (-> (with-inventory fresh-game [dagger])
              (type-key \w)
              (type-key \a))]
    (is (= {:main-hand dagger} (world/player-equipment (:world g))))
    (is (empty? (world/player-inventory (:world g))))
    (is (pos? (get-in g [:world :player :next-time])))
    (is (= "You are wielding a dagger (main hand)." (get-in g [:ui :current-msg])))))

(deftest wielding-with-both-hands-full-asks-which-hand
  (let [g (-> (with-inventory fresh-game [dagger-2])
              (with-equipment {:main-hand dagger :off-hand club})
              (type-key \w)
              (type-key \a))]
    (is (= :choice (:screen (game/current-mode g))))
    (is (= (get-in g [:world :player :next-time]) 0) "asking takes no time")
    (let [chose-off (type-key g \o)]
      (is (nil? (game/current-mode chose-off)))
      (is (= {:main-hand dagger :off-hand dagger-2}
             (world/player-equipment (:world chose-off))))
      (is (= [club] (world/player-inventory (:world chose-off)))))
    (let [chose-main (type-key g \m)]
      (is (= {:main-hand dagger-2 :off-hand club}
             (world/player-equipment (:world chose-main)))))))

(deftest any-other-key-cancels-the-hand-choice
  (let [g (-> (with-inventory fresh-game [dagger-2])
              (with-equipment {:main-hand dagger :off-hand club})
              (type-key \w)
              (type-key \a))
        cancelled (type-key g \x)]
    (is (nil? (game/current-mode cancelled)))
    (is (= (:world g) (:world cancelled)))))

(deftest e-opens-the-equipment-list
  (let [g (type-key (with-equipment fresh-game {:main-hand dagger}) \e)]
    (is (= :equipment-select (:screen (game/current-mode g))))
    (is (= g (type-key g \a)) "the equipment screen has no :on-select")))

(deftest e-with-nothing-equipped-shows-the-empty-message
  (let [g (type-key fresh-game \e)]
    (is (nil? (game/current-mode g)))
    (is (= "You are not wearing or wielding anything." (get-in g [:ui :current-msg])))))

(deftest t-then-a-letter-takes-off-that-slot
  (let [g (-> (with-equipment fresh-game {:main-hand dagger :off-hand club})
              (type-key \t))]
    (is (= g (type-key g \c)) "c) is the empty body slot")
    (let [took-off (type-key g \b)]
      (is (= {:main-hand dagger} (world/player-equipment (:world took-off))))
      (is (= [club] (world/player-inventory (:world took-off))))
      (is (= "You were wielding a club." (get-in took-off [:ui :current-msg]))))))
