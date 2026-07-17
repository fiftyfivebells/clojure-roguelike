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
    :wall "You bumped into a wall."
    :door "You found a closed door."
    :actor "There's someone there!"
    "Something's in your way."))
;; what is an event?
;; proposed shape:
;; :event/type -> the event type
;; TODO: what other things might be needed?
;;       not sure, but it'll probably become clearer as more events get added here
(defn apply-event
  [ui event]
  (case (:event/type event)
    :world/blocked (add-message ui (blocked-message (:by event)))

    ;; default case
    ui))

(defn apply-events
  [ui events]
  (reduce apply-event ui events))
