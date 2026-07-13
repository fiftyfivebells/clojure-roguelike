(ns roguelike.ui)

(defn new-ui
  []
  {:mode {:screen :play}
          :current-msg "Welcome to the dungeon."
          :messages []})

(defn update-mode
  [ui action]
  (let [action-type (name (:type action))]
    (case action-type
      "prompt"
      (let [{:keys [message on-yes return]} action]
        (assoc ui :mode {:screen :prompt
                         :message message
                         :on-yes on-yes
                         :return return}))

      "return"
      (assoc ui :mode (:return (:mode ui)))

      "quit"
      (assoc ui :mode {:screen :quit}))))

(defn add-message
  [ui message]
  (-> ui
      (assoc :current-msg message)
      (update :messages conj message)))

;; what is an event?
;; proposed shape:
;; :type -> the event type
;; TODO: what other things might be needed?
;;       not sure, but it'll probably become clearer as more events get added here
(defn apply-event
  [ui event]
  (case (:type event)
    :hit-impassable (add-message ui "You bumped into a wall.")

    ;; default case
    ui))
