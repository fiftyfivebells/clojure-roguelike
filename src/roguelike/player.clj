(ns roguelike.player)

(defn new-player
  "Builds the player entity at pos. :attacks are the player's unarmed attacks,
  used when no weapon is wielded."
  [pos]
  {:entity/id 0
   :entity/type :player
   :pos pos
   :sight/radius 4  ;; TODO: just a random magic number, fix to be more robust later
   :next-time 0
   :hp/current 15
   :hp/max 15
   :armor-class 0
   :evasion 10
   :to-hit 150
   :attacks [{:method :smash :effect :physical :dice 3 :sides 4}]
   :inventory []
   :equipment {}
   :knowledge {:item/known #{}}})
