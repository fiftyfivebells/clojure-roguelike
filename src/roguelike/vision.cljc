(ns roguelike.vision
  (:require
   [#?(:clj clojure.math
       :cljs   cljs.math) :as math]
   [roguelike.fov :as fov]
   [roguelike.level :as level]))

(defn- within?
  "Is the given [ox oy] coordinate within the radius of the given line of sight?"
  [[ox oy] radius [x y]]
  (let [dx (- x ox)
        dy (- y oy)]
    (<= (+ (* dx dx) (* dy dy)) (* radius radius))))

(defn- max-scan-radius
  [lvl]
  (let [[w h] (level/dimensions lvl)]
    (long (math/ceil (math/sqrt (+ (* w w) (* h h)))))))

(defn compute
  "Computes what the player at given pos can see. The :los key is every tile in
  line of sight at any range. :visible is what the player can actually see, ie.
  everything in the torch view or in a lit room."
  [lvl pos radius]
  (let [los (fov/visible-cells (partial level/opaque-at? lvl) pos (max-scan-radius lvl))
        lit (level/lit-tiles lvl)]
    {:origin pos
     :los los
     :lit lit
     :visible (into #{} (filter #(or (within? pos radius %) (contains? lit %))) los)}))

;; TODO: this works now, but will probably need to be refactored if I want to
;; introduce the concept of monsters that can see the player when the player
;; can't see them. Might need to refactor compute above as well/instead.
(defn sees?
  "Boolean value denoting whether an actor on the given pos can see the player.
  Since floor visibility is symmetric, if the player has line of sight to a pos,
  then that pos can see the player."
  [vision pos radius]
  (let [player-pos (:origin vision)]
    (and (contains? (:los vision) pos)
         (or (within? pos radius player-pos)
             (contains? (:lit vision) player-pos)))))
