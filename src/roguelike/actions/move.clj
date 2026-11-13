(ns roguelike.actions.move
  (:require [roguelike.combat :as combat]
            [roguelike.level :as level]
            [roguelike.world :as world]))

(defn- get-proposed-coords
  "Takes in a world, actor-id, and an [x y] delta. Then it finds the actor using the id and creates
  proposed coords based on the provided delta. Gives back the new coords."
  [world actor-id delta]
  (let [[x y] delta
        actor (world/get-actor world actor-id)
        [actor-x actor-y] (:pos actor)
        new-x (+ actor-x x)
        new-y (+ actor-y y)]
    [new-x new-y]))

(defn classify-destination
  "Classifies what's at the given coords for movement purposes: :passable, :wall,
  :door, or :actor. The default case returns :unknown."
  [world [x y]]
  (cond
    (world/entity-at world [x y]) :actor
    (world/walkable-at? world [x y]) :passable
    ;; Only reached when movement is already blocked: this case exists solely to
    ;; name the obstacle for the player-facing message.
    :else (case (level/classify-tile (world/tile-at world [x y]))
            :wall        :wall
            :closed-door :door
            :unknown)))

(defn observed-items
  [world items]
  (let [known (->> items
                   (map :item/kind)
                   (filter #(world/known-item? world %)))]

    {:event/type :world/items-seen
     :items items
     :known (set known)}))

(defn- apply-attack
  [world attack attacker defender]
  (let [base-result {:attacker (world/describe world attacker)
                     :defender (world/describe world defender)}]
    (if-not (:hit? attack)
      [world (merge base-result {:event/type :world/missed
                                 :hit? false
                                 :method (:method attack)})]
      (let [attack-result (merge attack base-result)]
        (if (:fatal? attack)
          [(world/resolve-death world (:defender base-result))
           (assoc attack-result :event/type :world/killed)]
          (let [new-hp (- (:hp/current defender) (:damage attack))
                world (world/update-actor world
                                    (:entity/id defender)
                                    #(assoc % :hp/current new-hp))]
            [world (assoc attack-result :event/type :world/hit)]))))))

(defn apply-attacks
  [world attacks attacker defender]
  (let [reducer (fn
                  [[world events] attack]
                  (let [defender (world/get-actor world (:entity/id defender))
                        [world event] (apply-attack world attack attacker defender)]
                    [world (conj events event)]))]
    (reduce reducer [world []] attacks)))

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
        (apply-attacks world resolved attacker defender)))))

(defn heal-player
  [world]
  (world/update-actor world (world/player-id world)
                (fn [player]
                  (update player :hp/current #(min (:hp/max player) (inc %))))))

(defn- attempt-movement
  [world actor-id {:keys [delta]}]
  (let [new-pos (get-proposed-coords world actor-id delta)
        destination (classify-destination world new-pos)
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
          [new-world [move-event (observed-items world items)]]
          [new-world [move-event]]))
      
      (= destination :actor)
      (attempt-attack world actor-id new-pos)

      :else
      [world [{:event/type :world/blocked :by destination :player? player?}]])))

(defn- wait
  [world actor-id _action]
  (let [world (if (world/player? world actor-id)
                (heal-player world)
                world)]
    [world [{:event/type :world/wait}]]))

(def actions
  {:world/move {:cost 10 :perform attempt-movement}
   :world/wait {:cost 10 :perform wait}})
