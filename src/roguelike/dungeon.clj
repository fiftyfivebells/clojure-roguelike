(ns roguelike.dungeon
  (:require [roguelike.level :as level]
            [roguelike.rng :as rng]))

;; Dungeon construction

;; ctx stands for context. A context is a container for generation:
;; {
;;  :rng-state -> internal rng-state that rng uses to geneerate random values
;;  :level -> the level the generator is making the dungeon for
;;  :rooms -> a list of rooms placed on the level
;; }

;; TODO: define min and max room width and height
;; for now, here's some pulled from the air values
(def ^:private min-w 5)
(def ^:private min-h 5)
(def ^:private max-w 15)
(def ^:private max-h 10)
(def ^:private room-margin 1)

(defn- propose-room
  "Creates one room with random height and width. Uses the ctx value's level
   property to ensure the room fits, then returns the context and the room."
  [ctx]
  (let [[w h] (level/dimensions (:level ctx))
        [ctx rw] (rng/draw-int ctx min-w max-w)
        [ctx rh] (rng/draw-int ctx min-h max-h)
        [ctx rx] (rng/draw-int ctx 1 (- w rw 1))
        [ctx ry] (rng/draw-int ctx 1 (- h rh 1))]
    [ctx {:bounds [rx ry rw rh] :lit? false}]))

(defn- rooms-overlap?
  "Two rooms overlap if their bounds intersect once room-a is padded by
   room-margin on every side, so placed rooms end up with at least that
   many empty tiles between them rather than sitting flush together."
  [room-a room-b]
  (let [[ax ay aw ah] (:bounds room-a)
        [bx by bw bh] (:bounds room-b)
        ax (- ax room-margin)
        ay (- ay room-margin)
        aw (+ aw (* 2 room-margin))
        ah (+ ah (* 2 room-margin))]
    (and (< ax (+ bx bw))
         (< bx (+ ax aw))
         (< ay (+ by bh))
         (< by (+ ay ah)))))

(defn- overlaps-any?
  "Takes in a ctx and room and determines whether the room overlaps with any
   of the rooms in the :room list from ctx."
  [ctx room]
  (some #(rooms-overlap? room %) (:rooms ctx)))

(defn- stamp-tile
  [level [x y]]
  (level/set-tile level [x y] (level/floor)))

(defn- stamp-tiles
  [level points]
  (reduce stamp-tile level points))

(defn- stamp-tiles-in-ctx
  [ctx points]
  (update ctx :level stamp-tiles points))

(defn- inclusive-range
  [a b]
  (range (min a b) (inc (max a b))))

(defn- stamp-floor
  "Iterates the area of a room inside the level and sets each of its tiles
   to be a bare floor."
  [level room]
  (let [[rx ry rw rh] (:bounds room)
        points (for [y (range ry (+ ry rh))
                     x (range rx (+ rx rw))]
                 [x y])]
    (stamp-tiles level points)))

(defn- place-rooms
  [ctx]
  (loop [attempts-remaining 300
         ctx ctx]
    (let [[ctx room] (propose-room ctx)]
      (cond
        (zero? attempts-remaining) ctx
        (overlaps-any? ctx room) (recur (dec attempts-remaining) ctx)
        :else
        (let [new-level (stamp-floor (:level ctx) room)
              rooms (conj (:rooms ctx) room)
              ctx (assoc ctx :rooms rooms :level new-level)]
          (recur (dec attempts-remaining) ctx))))))

(defn- stamp-horizontal-corridor
  [ctx x1 x2 y]
  (stamp-tiles-in-ctx ctx (for [x (inclusive-range x1 x2)] [x y])))

(defn- stamp-vertical-corridor
  [ctx y1 y2 x]
  (stamp-tiles-in-ctx ctx (for [y (inclusive-range y1 y2)] [x y])))

(defn- carve-l-corridor
  [ctx [x1 y1] [x2 y2]]
  (let [[ctx horizontal?] (rng/draw-boolean ctx)]
    (if horizontal?
      (-> ctx
          (stamp-horizontal-corridor x1 x2 y1)
          (stamp-vertical-corridor y1 y2 x2))

      (-> ctx
          (stamp-vertical-corridor y1 y2 x1)
          (stamp-horizontal-corridor x1 x2 y2)))))

(defn- room-center
  "Heler function to find the exact center of a room so we know where to draw
   the corridors from."
  [room]
  (let [[rx ry rw rh] (:bounds room)]
    [(+ rx (quot rw 2)) (+ ry (quot rh 2))]))

(defn- distance-sq
  "Computes the distance between two points in the grid."
  [[x1 y1] [x2 y2]]
  (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))

(defn- carve-corridors
  "Connects every room in ctx's :rooms list by repeatedly carving an L-shaped
   corridor from the current room to its nearest not-yet-visited neighbor,
   keeping corridors short instead of following placement order."
  [ctx]
  (let [rooms (:rooms ctx)]
    (if (< (count rooms) 2)
      ctx
      (loop [current (first rooms)
             remaining (rest rooms)
             ctx ctx]
        (if (empty? remaining)
          ctx
          (let [nearest (apply min-key
                               #(distance-sq (room-center current) (room-center %))
                               remaining)
                ctx (carve-l-corridor ctx (room-center current) (room-center nearest))]
            (recur nearest (remove #(identical? % nearest) remaining) ctx)))))))

(defn- run-passes
  [ctx passes]
  (reduce (fn [ctx pass] (pass ctx)) ctx passes))

(defn- finalize
  [ctx]
  (level/set-rooms (:level ctx) (:rooms ctx)))

;; Public API

(defn generate
  [world-seed level-id]
  (let [width 80  ;; TODO: this and height hard-coded for now, come back to this
        height 22
        lvl-seed (rng/mix world-seed :layout level-id)
        rng-state (rng/make lvl-seed)
        ctx {:rng-state rng-state :level (level/solid-level width height) :rooms []}]
    (run-passes ctx [place-rooms carve-corridors finalize])))
