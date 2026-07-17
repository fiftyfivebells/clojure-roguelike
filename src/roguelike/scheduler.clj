(ns roguelike.scheduler
  (:require [roguelike.ai :as ai]
            [roguelike.world :as world]))

(defn- cost-of
  [action]
  (case (:action/type action)
    :world/move 10
    :world/wait 10
    (throw (ex-info "this action doesn't exist" {:action action}))))

(defn- reschedule
  [world actor-id cost]
  (world/update-actor world actor-id #(assoc % :next-time (+ (:current-time world) cost))))

(defn resolve-action
  [world actor-id action]
  (let [[new-world events] (world/update-world world actor-id action)
        rescheduled-world (reschedule new-world actor-id (cost-of action))]
    [rescheduled-world events]))

(defn next-scheduled
  "Looks at a world and returns what is next up in the event scheduler. It could be the player, an entity,
  or simply advancing the game time one 'tick'. Returns a map with the info for whichever has the lowest
  next-time. The priority for breaking ties in lowest time is:
    - actors are always before tick
    - between actors, lowest id wins always
  This gives a priority (everything else equal) of player -> monster -> tick."
  [world]
  (let [make-scheduled-actor (fn [{:keys [entity/id next-time]}] {:kind :actor :entity/id id :at next-time})
        priority (fn [{:keys [kind at entity/id]}]
                   [at
                    (if (= kind :actor) 0 1)
                    (or id 0)])
        reducer (fn [best current]
                  (if (neg? (compare (priority current) (priority best)))
                    current
                    best))
        actors (map make-scheduled-actor (world/active-actors world))
        with-tick (conj actors {:kind :tick :at (:next-tick world)})]
    (reduce reducer with-tick)))

(defn tick-world
  [world]
  (assoc world :next-tick (+ (:current-time world) 10)))

(defn set-world-time
  [world time]
  (assoc world :current-time time))

(defn advance
  "Takes a world and advances it. This involves checking through the actors list for the next entity to act,
  then resolving whatever action that actor needs to take. Right now, this is:
  - player: returns an :awaiting-input status (needs player input at this point)
  - monster: uses monster ai to decide on some action for the monster to take
  - tick: 'ticks' the world forward to the next time unit"
  [world]
  (let [scheduled  (next-scheduled world)
        next-world (set-world-time world (:at scheduled))]
    (cond
      (= (:kind scheduled) :tick) [(tick-world next-world) [] :ticked]

      (world/player? world (:entity/id scheduled)) [next-world [] :awaiting-input]

      ;; else branch: decide the entity's next action and resolve it
      :else
      (let [[new-rng action] (ai/decide (:rng-state next-world) next-world (:entity/id scheduled))
            next-world (assoc next-world :rng-state new-rng)]
        (resolve-action next-world (:entity/id scheduled) action)))))
