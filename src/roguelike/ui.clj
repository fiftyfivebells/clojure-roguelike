(ns roguelike.ui)

(defn new-ui
  []
  {:mode {:screen :play}
   :current-msg "Welcome to the dungeon."
   :messages []})

(defn update-mode
  [ui action]
  (case (:action/type action)
    :ui/prompt
    (let [{:keys [message on-yes return]} action]
      (assoc ui :mode {:screen :prompt
                       :message message
                       :on-yes on-yes
                       :return return}))

    :ui/return
    (assoc ui :mode (:return (:mode ui)))

    :ui/quit
    (assoc ui :mode {:screen :quit})

    ;; default no-ops: just return the ui unchanged
    ui))

(defn add-message
  [ui message]
  (-> ui
      (assoc :current-msg message)
      (update :messages conj message)))

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

;; what is an event?
;; proposed shape:
;; :event/type -> the event type
;; TODO: what other things might be needed?
;;       not sure, but it'll probably become clearer as more events get added here
(defn apply-event
  [ui event]
  (case (:event/type event)
    :world/travelled
    (add-message ui (traveled-message (:direction event) (:depth event)))

    :world/invalid-action
    (add-message ui (invalid-action-message (:action event)))

    :world/blocked
    (if (:player? event)
      (add-message ui (blocked-message (:by event)))
      ui)

    :world/wait
    (add-message ui "You sit patiently and wait.")

    ;; default case
    ui))

(defn apply-events
  [ui events]
  (reduce apply-event ui events))
