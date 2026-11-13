(ns roguelike.actions.equipment
  (:require [roguelike.equipment :as equipment]
            [roguelike.inventory :as inventory]
            [roguelike.world :as world]))

(defn- wield-item
  "The player wields or wears the item with the given id. It leaves the pack
  and goes into slot, or into the slot equipment/slot-for picks when slot is
  nil. Anything it displaces goes back into the pack. A one-handed weapon with
  both hands full is ambiguous, so the caller is required to pass a slot.
  The :world/wielded event carries the item, its slot, and the displaced items."
  [world _actor-id {:keys [item-id slot]}]
  (let [item (inventory/find-item-by-id (world/player-inventory world) item-id)]
    (when (nil? item)
      (throw (ex-info "no item with that id in the player's inventory" {:item/id item-id})))
    (let [slot (or slot (equipment/slot-for (world/player-equipment world) item))]
      (when (= :ambiguous slot)
        (throw (ex-info "both hands are full; the slot must be chosen" {:item/id item-id})))
      (let [[equipment displaced] (equipment/equip (world/player-equipment world) item slot)
            world (world/update-actor world (world/player-id world)
                                #(-> %
                                     (assoc :equipment equipment)
                                     (update :inventory inventory/remove-stack item-id)
                                     (update :inventory inventory/add-items displaced)))]
        [world [{:event/type :world/wielded :item item :slot slot
                 :removed displaced :known (world/known-items world)}]]))))

(defn- take-off
  "The player takes off whatever is in slot and puts it back in the pack. The
  :world/took-off event carries the item and the slot it came from."
  [world _actor-id {:keys [slot]}]
  (let [[equipment item] (equipment/unequip (world/player-equipment world) slot)]
    (when (nil? item)
      (throw (ex-info "nothing is equipped in that slot" {:slot slot})))
    [(world/update-actor world (world/player-id world)
                   #(-> %
                        (assoc :equipment equipment)
                        (update :inventory inventory/add-item item)))
     [{:event/type :world/took-off :item item :slot slot :known (world/known-items world)}]]))

(def actions
  {:world/wield    {:cost 5 :perform wield-item}
   :world/take-off {:cost 5 :perform take-off}})

