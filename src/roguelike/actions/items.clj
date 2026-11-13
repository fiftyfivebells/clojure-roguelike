(ns roguelike.actions.items
  (:require [roguelike.inventory :as inventory]
            [roguelike.level :as level]
            [roguelike.world :as world]))

;; TODO: currently, if we drop an item, and then we drop another of the exact
;; same item, the ui only shows 1 of the items when the player stands on the
;; stack. When the player 'g'ets the stack, both get picked up, but it's a small
;; inconsistency. I should fix this later.
(defn- drop-item
  "Drops quantity items from the stack with the given id onto the floor under
  the actor. Dropping part of a stack splits it off as a new item with its own
  id. The :world/dropped event carries the item as it landed on the floor."
  [world actor-id {:keys [item-id quantity]}]
  (let [pos  (:pos (world/get-actor world actor-id))
        item (inventory/find-item-by-id (world/player-inventory world) item-id)
        [dropped world] (if (< quantity (:item/quantity item))
                          (let [[id world] (world/allocate-entity-id world)]
                            [(assoc item :item/id id :item/quantity quantity) world])
                          [item world])
        dropped (assoc dropped :item/pos pos)
        world   (-> world
                    (world/update-actor (world/player-id world)
                                        #(update % :inventory inventory/remove-quantity item-id quantity))
                    (world/update-current-level #(level/add-item % pos dropped)))]
    [world [{:event/type :world/dropped :item dropped :known (world/known-items world)}]]))

;; TODO: this function currently emits one event that carries only the first
;; item that was picked up. This is fine if the player picks up a single item,
;; or a stack of the same item, but it loses info if there are multiple items
;; picked up. Say the player picks up 2 potions and a dagger. This function only
;; carries the first item picked up, so we only send one message. I want to
;; refactor this eventually to emit messages for all the items, but this is part
;; of a larger refactor of the message log.
(defn- attempt-pickup
  [world actor-id _action]
  (let [current-pos (:pos (world/get-actor world actor-id))
        items (level/items-at (world/current-level world) current-pos)]
    (if (seq items)
      (let [new-world (-> world
                          (world/update-player-inventory items)
                          (world/update-current-level #(level/remove-items % current-pos)))
            new-item (inventory/find-item (world/player-inventory new-world) (first items))]
        [new-world [{:event/type :world/picked-up :item new-item :known (world/known-items world)}]])
      [world [{:event/type :world/pickup-failed :reason :no-item}]])))

(defn- pack-label
  "The letter the item with this id has in the player's pack, or nil."
  [world item-id]
  (some #(when (= item-id (:item/id (:item %))) (:label %)) (world/player-pack world)))


(defn- use-item
  "The player uses one item from the stack with the given id. The item is used
  up, its kind becomes known, and its effects are applied. The :world/<verb>
  event carries the item as it was before using, whether this use identified it,
  and the stack's letter (where it sits now, or where it was if none are left);
  effect events follow it."
  [world item-id event-type]
  (let [item (inventory/find-item-by-id (world/player-inventory world) item-id)]
    (when (nil? item)
      (throw (ex-info "no item with that id in the player's inventory" {:item/id item-id})))
    (let [kind (:item/kind item)
          learned? (not (world/known-item? world kind))
          label-before (pack-label world item-id)
          world (-> world
                    (world/update-actor (world/player-id world) #(update % :inventory inventory/remove-one item-id))
                    (world/learn-item-kind kind))
          label (or (pack-label world item-id) label-before)
          [world effect-events] (world/apply-effects world (world/player-id world) (:item/effects item))]
      [world (into [{:event/type event-type :item item :learned? learned? :label label}]
                   effect-events)])))

(defn- quaff [world _actor-id {:keys [item-id]}]
  (use-item world item-id :world/quaffed))

(defn- read-scroll [world _actor-id {:keys [item-id]}]
  (use-item world item-id :world/read-scroll))

(def actions
  {:world/get   {:cost 5  :perform attempt-pickup}
   :world/drop  {:cost 5  :perform drop-item}
   :world/quaff {:cost 10 :perform quaff}
   :world/read  {:cost 10 :perform read-scroll}})
