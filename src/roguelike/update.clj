(ns roguelike.update
  (:require [roguelike.world :as world]))

(defmulti bump-action
  (fn [_world destination-tile _coords]
    (get-in destination-tile [:tile :type])))

(defmethod bump-action :floor
  [world _tile destination-coords]
      (world/move-player world destination-coords))

(defn update-world
  [world input]
  (let [world (assoc world :current-msg "")]
    (case (:type input)
      :move (let [delta [(:dx input) (:dy input)]
                  proposed-coords (world/get-proposed-coords world delta)
                  tile (world/get-tile world proposed-coords)]
              (bump-action world tile delta))

      :prompt (world/set-mode world {:screen  :prompt
                                     :message (:message input)
                                     :on-yes  (:on-yes input)
                                     :return  (:return input)})

      :return (world/set-mode world (:return (:mode world)))

      :quit (world/set-mode world :quit)
      :none world)))
