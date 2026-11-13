(ns roguelike.actions
  (:require [roguelike.actions.equipment :as equipment]
            [roguelike.actions.items :as items]
            [roguelike.actions.move :as move]
            [roguelike.actions.travel :as travel]))

(defn- combine
  "Merges action tables, throwing if two tables register the same action.
  Plain merge would silently let the later one win."
  [& tables]
  (reduce (fn [acc table]
            (when-let [dups (seq (filter (set (keys acc)) (keys table)))]
              (throw (ex-info "action registered twice" {:actions dups})))
            (merge acc table))
          {}
          tables))

(def registry
  (combine move/actions travel/actions items/actions equipment/actions))

(defn- lookup [action]
  (or (get registry (:action/type action))
      (throw (ex-info "unregistered action" {:action action}))))

(defn cost [action]
  (:cost (lookup action)))

(defn perform [world actor-id action]
  ((:perform (lookup action)) world actor-id action))
