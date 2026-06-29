(ns roguelike.world)

(defn new-world
  []
   {:player {:x 10 :y 12}
    :width 80
    :play-start-row 1
    :map-rows 22
    :msg-row 23
    :current-msg "Welcome to the dungeon."
    :messages ["Welcome to the dungeon."]
    :height 24
    :mode {:screen :play}})

(defn clamp-position
  [world {new-x :x new-y :y}]
  {:x (max 0 (min new-x (- (:width world) 1)))
   :y (max 0 (min new-y (- (:map-rows world) 1)))})

(defn add-message
  [world message]
  (-> world
      (assoc :current-msg message)
      (update :messages conj message)))

(defn get-walking-message
  [delta]
  (letfn [(find-direction [{dx :x dy :y}]
            (case [dx dy]
              [0 -1] "north"
              [0 1] "south"
              [1 0] "east"
              [-1 0] "west"))]
    (str "You take a step " (find-direction delta) ".")))

(defn move-player
  [world delta]
  (let [player-pos (:player world)
        new-x (+ (:x player-pos) (:x delta))
        new-y (+ (:y player-pos) (:y delta))
        new-pos (clamp-position world {:x new-x :y new-y})]
    (assoc world :player new-pos)))

(defn set-mode
  [world mode]
  (assoc world :mode mode))
