(ns roguelike.ui 
  (:require
    [clojure.string :as str]
    [roguelike.ui.appearance.monster :as monster]
    [roguelike.ui.appearance.item :as item]
    [roguelike.ui.equipment-select :as equipment-select]
    [roguelike.ui.number-select :as number-select]))

(defn new-ui
  []
  {:modes []
   :current-msg "Welcome to the dungeon."
   :messages []})

(defn current-mode
  [ui]
  (peek (:modes ui)))

(defn push-mode
  [ui frame]
  (update ui :modes conj frame))

(defn pop-mode
  [ui]
  (update ui :modes pop))

(defn- update-current-mode
  [ui f]
  (update ui :modes #(conj (pop %) (f (peek %)))))

(defn update-mode
  [ui action]
  (case (:action/type action)
    :ui/prompt
    (let [{:keys [message on-yes]} action]
      (push-mode ui {:screen :prompt
                     :message message
                     :on-yes on-yes}))

    :ui/number-select
    (push-mode ui (-> action
                      (dissoc :action/type)
                      (assoc :screen :number-select :text "")))

    :ui/choice
    (push-mode ui (-> action
                      (dissoc :action/type)
                      (assoc :screen :choice)))

    :ui/type
    (update-current-mode ui #(update % :text number-select/type-char (:char action)))

    :ui/erase
    (update-current-mode ui #(update % :text number-select/erase))

    :ui/return
    (pop-mode ui)

    :ui/quit
    (push-mode ui {:screen :quit})

    ;; default no-ops: just return the ui unchanged
    ui))

(defn add-message
  [ui message]
  (-> ui
      (assoc :current-msg message)
      (update :messages conj message)))

(defn clear-message
  [ui]
  (assoc ui :current-msg nil))

(defn- attack-message
  [attack]
  (let [player? (get-in attack [:attacker :player?])
        monster-name (monster/name-of
                      (:species (get attack (if player? :defender :attacker))))
        atk-name (if player?
                   "Your"
                   (str "The " monster-name "'s"))
        method (name (:method attack))
        target (if-not player?
                 "you"
                 (str "the " monster-name))]
    (if-not (:hit? attack)
      (str atk-name " " method " missed " target ".")
      (str atk-name " " method " hits " target "."))))

(defn- blocked-message
  [by]
  (case by
    :wall  "You bumped into a wall."
    :door  "You found a closed door."
    :actor "There's someone there!"
    "Something's in your way."))

(defn- invalid-action-message
  [by]
  (case by
    :stairs-up "You can't go up here!"
    :stairs-down "You can't go down here!"
    "You can't do what you just tried!"))

(defn- traveled-message
  [direction depth]
  (let [dir-msg (case direction
                  :down "descend"
                  :up   "ascend"
                  "travel")]
    (str "You " dir-msg " to level " depth ".")))

(defn- remaining-message
  "What's left after using an item, like Angband: 'You have 2 Potions of Harm
  (a).' or 'You have no more Potions of Harm (a).' The item is known by now, so
  it always shows its real name."
  [{:keys [item label]} flavors]
  (let [remaining (update item :item/quantity dec)
        known #{(:item/kind item)}]
    (str "You have " (item/describe remaining flavors known) " (" label ").")))

(defn- healed-message
  "Angband's healing messages, chosen by how much was healed."
  [amount]
  (cond
    (< amount 5)  "You feel a little better."
    (< amount 15) "You feel better."
    (< amount 35) "You feel much better."
    :else         "You feel very good."))

(defn- dropped-message
  [{:keys [item known]} flavors]
  (str "You dropped " (item/describe item flavors known) "."))

(defn- picked-up-message
  [{:keys [item known]} flavors]
  (str "You have " (item/describe item flavors known) "."))

(defn- wear-verb
  "Weapons are wielded, everything else is worn."
  [item]
  (if (:item/hands item) "wielding" "wearing"))

(defn- were-message
  [item flavors known]
  (str "You were " (wear-verb item) " " (item/describe item flavors known) "."))

(defn- wielded-message
  "Constructs a message to display what the user has wielded/worn."
  [{:keys [item slot removed known]} flavors]
  (let [weapon? (:item/hands item)
        now (str "You are " (wear-verb item) " " (item/describe item flavors known)
                 (when weapon? (str " (" (equipment-select/slot-names slot) ")"))
                 ".")]
    (str/join " " (concat (map #(were-message % flavors known) removed) [now]))))

(defn- took-off-message
  [{:keys [item known]} flavors]
  (were-message item flavors known))

(def ^:private silent-events #{:world/moved :world/teleported})

;; what is an event?
;; proposed shape:
;; :event/type -> the event type
;; TODO: what other things might be needed?
;;       not sure, but it'll probably become clearer as more events get added here
(defn apply-event
  [ui event flavors]
  (case (:event/type event)
    :world/travelled
    (add-message ui (traveled-message (:direction event) (:depth event)))

    :world/invalid-action
    (add-message ui (invalid-action-message (:action event)))

    :world/items-seen
    (let [known (:known event)
          items (:items event)]
      (add-message ui (str "You see " (item/describe (first items) flavors known) ".")))

    :world/blocked
    (if (:player? event)
      (add-message ui (blocked-message (:by event)))
      ui)

    :world/hit
    (add-message ui (attack-message event))

    :world/missed
    (add-message ui (attack-message event))

    :world/killed
    (if (get-in event [:defender :player?])
      (-> ui
          (add-message "You have died. Press any key to continue.")
          (push-mode {:screen :dead}))
      (add-message ui (str "The " (monster/name-of (:species (:defender event))) " dies.")))

    :world/wait
    (add-message ui "You sit patiently and wait.")

    :world/dropped
    (add-message ui (dropped-message event flavors))

    :world/picked-up
    (add-message ui (picked-up-message event flavors))

    :world/wielded
    (add-message ui (wielded-message event flavors))

    :world/took-off
    (add-message ui (took-off-message event flavors))

    (:world/quaffed :world/read-scroll)
    (add-message ui (remaining-message event flavors))

    :world/healed
    (add-message ui (healed-message (:amount event)))

    :world/damaged
    (add-message ui "You feel very sick.")

    ;; default case
    (if (silent-events (:event/type event))
      ui
      (add-message ui (str "[no message for " (:event/type event) "]")))))

(defn apply-events
  [ui events flavors]
  (reduce (fn [ui event] (apply-event ui event flavors)) ui events))
