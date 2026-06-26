(ns roguelike.world)

(defn new-world
  []
  {:player {:x 10 :y 12}
   :width 80
   :height 24
   :mode :play})

(defn clamp-position
  [world {new-x :x new-y :y}]
  {:x (max 0 (min new-x (- (:width world) 1)))
   :y (max 0 (min new-y (- (:height world) 1)))})

(defn move-player
  [world delta]
  (let [player-pos (:player world)
        new-x (+ (:x player-pos) (:x delta))
        new-y (+ (:y player-pos) (:y delta))
        new-pos (clamp-position world {:x new-x :y new-y})]
    (assoc-in world [:player] new-pos)))
