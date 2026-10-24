(ns roguelike.game
  (:require [roguelike.scheduler :as scheduler]
            [roguelike.world :as world]
            [roguelike.ui    :as ui]
            [roguelike.ui.item-select :as item-select]))

(defn new-game
  []
  {:world (world/new-world)
   :ui    (ui/new-ui)})

(defn current-mode
  [game]
  (ui/current-mode (:ui game)))

(defn advance
  [game]
  (let [[new-world events status] (scheduler/advance (:world game))
        observed                  (world/observe new-world)]
    [(-> game
         (assoc :world observed)
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

(defn- resolve-selection
  "Turns a letter picked in the item picker (:ui/select) into the picker's
  :on-select action with :item-id attached, closing the picker. Letters that
  aren't on screen, or a picker with no :on-select, become :ui/none. Any other
  action passes through unchanged. Returns [game action]."
  [game action]
  (if-not (= :ui/select (:action/type action))
    [game action]
    (let [frame (current-mode game)
          entry (item-select/find-entry (selectable game frame) (:label action))]
      (if (and entry (:on-select frame))
        [(update game :ui ui/pop-mode)
         (assoc (:on-select frame) :item-id (:item/id (:item entry)))]
        [game {:action/type :ui/none}]))))

(defn player-action
  [game action]
  (let [[game action] (resolve-selection game action)]
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
        (update game :ui ui/update-mode action))

      (throw (ex-info "unroutable action" {:action action})))))
