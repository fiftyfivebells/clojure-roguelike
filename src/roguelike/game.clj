(ns roguelike.game
  (:require [roguelike.equipment :as equipment]
            [roguelike.scheduler :as scheduler]
            [roguelike.world :as world]
            [roguelike.ui    :as ui]
            [roguelike.ui.equipment-select :as equipment-select]
            [roguelike.ui.item-select :as item-select]
            [roguelike.ui.number-select :as number-select]))

(defn new-game
  []
  {:world (world/new-world)
   :ui    (ui/new-ui)})

(defn current-mode
  [game]
  (ui/current-mode (:ui game)))

(defn advance
  [game]
  (let [[new-world events status] (scheduler/advance (:world game))]
    [(-> game
         (assoc :world new-world)
         (update :ui ui/apply-events events (world/flavors new-world)))
     status]))

(defn- selectable
  [game frame]
  (item-select/selectable (world/player-pack (:world game)) (:filter frame)))

(defn- open-item-select
  "Opens the item picker described by the action (see roguelike.ui.item-select),
  or shows its :empty message if no item fits."
  [game action]
  (let [frame (-> action (dissoc :action/type) (assoc :screen :item-select))]
    (if (empty? (selectable game frame))
      (update game :ui ui/add-message (:empty frame))
      (update game :ui ui/push-mode frame))))


(defn- ask-quantity
  "The action to send once an item is picked. If the picker asks for a
  quantity (:quantity-prompt) and the stack has more than one, opens the
  number picker; a single item skips it and goes out with :quantity 1."
  [frame item selected]
  (let [quantity (:item/quantity item)]
    (cond
      (not (:quantity-prompt frame)) selected
      (= 1 quantity) (assoc selected :quantity 1)
      :else {:action/type :ui/number-select
             :prompt      (:quantity-prompt frame)
             :max         quantity
             :on-select   selected})))

(defn- ask-hand
  "The action to send once an item is picked. Wielding a one-handed weapon with
  both hands full opens a choice prompt for which hand to use."
  [game item selected]
  (if (and (= :world/wield (:action/type selected))
           (= :ambiguous (equipment/slot-for (world/player-equipment (:world game)) item)))
    {:action/type :ui/choice
     :prompt      "Wield it in which hand? (m)ain or (o)ff"
     :choices     {\m (assoc selected :slot :main-hand)
                   \o (assoc selected :slot :off-hand)}}
    selected))

(defn- resolve-selection
  "Turns a letter picked in the item picker (:ui/select) into the picker's
  :on-select action with :item-id attached, closing the picker (or into a
  number picker or hand choice first, see ask-quantity and ask-hand). Letters
  that aren't on screen, or a picker with no :on-select, become :ui/none. Any
  other action passes through unchanged. Returns [game action]."
  [game action]
  (let [frame (current-mode game)]
    (if-not (and (= :ui/select (:action/type action))
                 (= :item-select (:screen frame)))
      [game action]
      (let [entry (item-select/find-entry (selectable game frame) (:label action))]
        (if (and entry (:on-select frame))
          (let [item (:item entry)
                selected (assoc (:on-select frame) :item-id (:item/id item))]
            [(update game :ui ui/pop-mode)
             (->> selected
                  (ask-quantity frame item)
                  (ask-hand game item))])
          [game {:action/type :ui/none}])))))

(defn- open-equipment-select
  "Opens the equipment list described by the action (see
  roguelike.ui.equipment-select), or shows its :empty message if nothing is
  equipped."
  [game action]
  (let [frame (-> action (dissoc :action/type) (assoc :screen :equipment-select))]
    (if (empty? (world/player-equipment (:world game)))
      (update game :ui ui/add-message (:empty frame))
      (update game :ui ui/push-mode frame))))

(defn- resolve-slot
  "Turns a letter picked in the equipment list (:ui/select) into the list's
  :on-select action with :slot attached, closing the list. Empty slots, letters
  that aren't on screen, or a list with no :on-select become :ui/none. Any
  other action passes through unchanged. Returns [game action]."
  [game action]
  (let [frame (current-mode game)]
    (if-not (and (= :ui/select (:action/type action))
                 (= :equipment-select (:screen frame)))
      [game action]
      (let [entries (equipment-select/entries (world/player-equipment (:world game)))
            entry (equipment-select/find-entry entries (:label action))]
        (if (and (:item entry) (:on-select frame))
          [(update game :ui ui/pop-mode)
           (assoc (:on-select frame) :slot (:slot entry))]
          [game {:action/type :ui/none}])))))

(defn- resolve-choice
  "Turns a key pressed at a choice prompt (:ui/choose) into the matching
  action from the prompt's :choices, closing the prompt. A key that isn't a
  choice just closes it. Any other action passes through unchanged. Returns
  [game action]."
  [game action]
  (if-not (= :ui/choose (:action/type action))
    [game action]
    (let [frame (current-mode game)]
      [(update game :ui ui/pop-mode)
       (get (:choices frame) (:label action) {:action/type :ui/none})])))

(defn- resolve-number
  "Turns Enter in the number picker (:ui/submit) into the picker's :on-select
  action with :quantity attached, closing the picker. Any other action passes
  through unchanged. Returns [game action]."
  [game action]
  (if-not (= :ui/submit (:action/type action))
    [game action]
    (let [frame (current-mode game)]
      [(update game :ui ui/pop-mode)
       (assoc (:on-select frame) :quantity (number-select/value frame))])))

(defn player-action
  [game action]
  (let [[game action] (resolve-selection game action)
        [game action] (resolve-slot game action)
        [game action] (resolve-number game action)
        [game action] (resolve-choice game action)]
    (case (namespace (:action/type action))
      "world"
      (let [[new-world events] (scheduler/player-action (:world game) action)
            new-ui             (ui/apply-events (ui/clear-message (:ui game)) events (world/flavors new-world))]
        (-> game
            (assoc :world new-world)
            (assoc :ui new-ui)))

      "ui"
      (case (:action/type action)
        :ui/item-select (open-item-select game action)
        :ui/equipment-select (open-equipment-select game action)
        (update game :ui ui/update-mode action))

      (throw (ex-info "unroutable action" {:action action})))))
