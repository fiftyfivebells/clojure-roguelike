(ns roguelike.update
  (:require [roguelike.world :as world]))

(defmulti bump-action
  "Multi method for dispatching to different functions to determing the behavior depending on tile type."
  (fn [_world destination-tile _coords]
    (get-in destination-tile [:tile :type])))

(defmethod bump-action :floor
  [world _tile destination-coords]
  (world/move-player world destination-coords))

(defmethod bump-action :wall
  [world _tile _destination-coords]
  (let [msg "You bumped into a wall."]
    (world/add-message world msg)))

(defn update-world
  "Overall update function. This is pure; it takes in a 'world' map and an input, then dispatches to the
  appropriate function and returns an updated world. Does not mutate the original world, but produces a
  new world with the appropriate update applied."
  [world input]
  (let [world (assoc-in world [:ui :current-msg] "")]
    (case (:type input)
      :move (let [delta [(:dx input) (:dy input)]
                  proposed-coords (world/get-proposed-coords world delta)
                  tile (world/get-tile world proposed-coords)]
              (bump-action world tile proposed-coords))

      :prompt (world/set-mode world {:screen  :prompt
                                     :message (:message input)
                                     :on-yes  (:on-yes input)
                                     :return  (:return input)})

      :return (world/set-mode world (:return (:mode (:ui world))))

      :quit (world/set-mode world :quit)
      :none world)))
