(ns roguelike.actions.move
  (:require [roguelike.combat :as combat]
            [roguelike.world :as world]))

(defn- attempt-attack
  [world actor-id new-pos]
  (let [attacker (world/get-actor world actor-id)
        defender (world/entity-at world new-pos)]
    (if (and (not (world/player? world actor-id))
             (not (world/player? world (:entity/id defender))))
      [world [{:event/type :world/blocked :by :actor :player? false}]]
      (let [atk-profile (combat/attacker-profile attacker)
            def-profile (combat/defender-profile defender)
            [world resolved] (combat/resolve-attacks world atk-profile def-profile) ]
        (world/apply-attacks world resolved attacker defender)))))

(defn- attempt-movement
  [world actor-id {:keys [delta]}]
  (let [new-pos (world/get-proposed-coords world actor-id delta)
        destination (world/classify-destination world new-pos)
        player? (world/player? world actor-id)]
    (cond
      ;; TODO: here, I think I need to add some kind of a flag to show whether
      ;; player is on items or not. If the player is moving onto items, then we
      ;; need to display a message (ie. "You see a <whatever> here."). Probably
      ;; something like :items? on the event so the ui knows whether to add a
      ;; message or proceed as normal
      (= destination :passable)
      (let [new-world (world/move-actor world actor-id new-pos)
            move-event {:event/type
                        :world/moved
                        :player? player?}
            items (world/items-at new-world new-pos)]
        (if (and player? items)
          [new-world [move-event (world/observed-items world items)]]
          [new-world [move-event]]))
      
      (= destination :actor)
      (attempt-attack world actor-id new-pos)

      :else
      [world [{:event/type :world/blocked :by destination :player? player?}]])))

(defn- wait
  [world actor-id _action]
  (let [world (if (world/player? world actor-id)
                (world/heal-player world)
                world)]
    [world [{:event/type :world/wait}]]))

(def actions
  {:world/move {:cost 10 :perform attempt-movement}
   :world/wait {:cost 10 :perform wait}})
