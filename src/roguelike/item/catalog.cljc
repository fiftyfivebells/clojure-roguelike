(ns roguelike.item.catalog)

(def ^:private potion-defaults
  {:item/stackable? true
   :item/verb :quaff})

(def ^:private potions
  {:potion/lesser-healing {:item/effects [{:effect/type :heal :dice 2 :sides 4}]
                           :item/rarity 8}

   :potion/harm {:item/effects [{:effect/type :damage :dice 2 :sides 4}]
                 :item/rarity 8}})

(def ^:private scroll-defaults
  {:item/stackable? true
   :item/verb :read})

(def ^:private scrolls
  {:scroll/phase-door {:item/effects [{:effect/type :teleport :dice 2 :sides 5}]
                       :item/rarity 8}})

(def ^:private edged
  {:edged/dagger {:item/damage {:dice 1 :sides 4}
                  :item/method :thrust
                  :item/weight 12}

   :edged/longsword {:item/damage {:dice 2 :sides 5}
                     :item/method :slash
                     :item/weight 120}})

(def ^:private hafted
  {:hafted/club {:item/damage {:dice 1 :sides 5}
                 :item/method :smash
                 :item/weight 50}})

(def ^:private polearm
  {:polearm/spear {:item/damage {:dice 1 :sides 8}
                   :item/method :thrust
                   :item/weight 50}})

(def ^:private weapon-defaults
  {:item/stackable? false
   :item/slot :weapon})

(def items
  {:potion  (merge potion-defaults {:kinds potions})
   :scroll  (merge scroll-defaults {:kinds scrolls})
   :edged   (merge weapon-defaults {:kinds edged})
   :hafted  (merge weapon-defaults {:kinds hafted})
   :polearm (merge weapon-defaults {:kinds polearm})})
