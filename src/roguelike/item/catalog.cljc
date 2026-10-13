(ns roguelike.item.catalog)

(def ^:private potion-defaults
  {:item/stackable? true
   :item/verb :quaff})

(def ^:private potions
  {:potion/lesser-healing {:item/effects [{:effect/type :heal :dice 2 :sides 4}]
                           :item/alloc {:commonness 80 :from 0 :to 15}}

   :potion/harm {:item/effects [{:effect/type :damage :dice 2 :sides 4}]
                 :item/alloc {:commonness 25 :from 0 :to 15}}})

(def ^:private scroll-defaults
  {:item/stackable? true
   :item/verb :read})

(def ^:private scrolls
  {:scroll/phase-door {:item/effects [{:effect/type :teleport :dice 2 :sides 5}]
                       :item/alloc {:commonness 50 :from 0}}})

(def ^:private edged
  {:edged/dagger {:item/damage {:dice 1 :sides 4}
                  :item/method :thrust
                  :item/weight 12
                  :item/alloc {:commonness 80 :from 0}}

   :edged/longsword {:item/damage {:dice 2 :sides 5}
                     :item/method :slash
                     :item/weight 120
                     :item/alloc {:commonness 40 :from 1}}})

(def ^:private hafted
  {:hafted/club {:item/damage {:dice 1 :sides 5}
                 :item/method :smash
                 :item/weight 50
                 :item/alloc {:commonness 70 :from 0}}})

(def ^:private polearm
  {:polearm/spear {:item/damage {:dice 1 :sides 8}
                   :item/method :thrust
                   :item/weight 50
                   :item/alloc {:commonness 60 :from 1}}})

(def ^:private weapon-defaults
  {:item/stackable? false
   :item/slot :weapon})

(def items
  {:potion  (merge potion-defaults {:kinds potions})
   :scroll  (merge scroll-defaults {:kinds scrolls})
   :edged   (merge weapon-defaults {:kinds edged})
   :hafted  (merge weapon-defaults {:kinds hafted})
   :polearm (merge weapon-defaults {:kinds polearm})})
