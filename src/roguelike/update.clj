(ns roguelike.update
  (:require [roguelike.world :as world]))

(defn update-world
  [world input]
  (let [world (assoc world :current-msg "")]
    (case (:type input)
      :move (let [delta {:x (:dx input) :y (:dy input)}
                  moved (world/move-player world delta)
                  collision? (= (:player moved) (:player world))]
              (cond-> moved
                collision? (world/add-message "You ran into a wall.")))

      :prompt (world/set-mode world {:screen :prompt
                                     :message (:message input)
                                     :on-yes (:on-yes input)
                                     :return (:return input)})

      :return (world/set-mode world (:return (:mode world)))

      :quit (world/set-mode world :quit)
      :none world)))
