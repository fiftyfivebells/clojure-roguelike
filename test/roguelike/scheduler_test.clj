(ns roguelike.scheduler-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.scheduler :as scheduler]
            [roguelike.level :as level]
            [roguelike.ai :as ai]
            [roguelike.rng :as rng]))

;;; Helpers

(defn- make-player
  [overrides]
  (merge {:entity/id 0 :entity/type :player :pos [1 1] :next-time 0} overrides))

(defn- make-monster
  [id overrides]
  (merge {:entity/id id :entity/type :generic-monster :pos [2 2] :next-time 0} overrides))

(defn- make-world
  "Builds a minimal world for scheduler tests: an 80x22 test-level bordered by walls,
  a player, and any monsters supplied."
  [& {:keys [player monsters current-time next-tick]
      :or   {player (make-player {}) monsters [] current-time 0 next-tick 10}}]
  (let [current-level (reduce level/add-entity (level/test-level) monsters)]
    {:player         player
     :current-level  current-level
     :next-tick      next-tick
     :current-time   current-time
     :rng-state      (rng/make 42)
     :next-entity-id (inc (apply max 0 (map :entity/id monsters)))}))

;;; cost-of

(deftest cost-of-move-is-10
  (is (= 10 (#'scheduler/cost-of {:action/type :world/move :delta [1 0]}))))

(deftest cost-of-wait-is-10
  (is (= 10 (#'scheduler/cost-of {:action/type :world/wait}))))

(deftest cost-of-unknown-action-throws
  (is (thrown? clojure.lang.ExceptionInfo (#'scheduler/cost-of {:action/type :bogus}))))

;;; reschedule

(deftest reschedule-sets-next-time-for-player
  (let [w      (make-world :current-time 50)
        result (#'scheduler/reschedule w 0 10)]
    (is (= 60 (:next-time (:player result))))))

(deftest reschedule-sets-next-time-for-monster
  (let [m      (make-monster 1 {})
        w      (make-world :monsters [m] :current-time 30)
        result (#'scheduler/reschedule w 1 10)]
    (is (= 40 (:next-time (level/get-entity (:current-level result) 1))))))

(deftest reschedule-does-not-affect-other-actors
  (let [m      (make-monster 1 {:next-time 5})
        w      (make-world :monsters [m] :current-time 0)
        result (#'scheduler/reschedule w 0 10)]
    (is (= 5 (:next-time (level/get-entity (:current-level result) 1))) "monster untouched")))

;;; resolve-action

(deftest resolve-action-wait-reschedules-and-returns-no-events
  (let [w                    (make-world :current-time 0)
        [new-world events]   (scheduler/resolve-action w 0 {:action/type :world/wait})]
    (is (= [] events))
    (is (= 10 (:next-time (:player new-world))))))

(deftest resolve-action-move-success-moves-actor-and-reschedules
  (let [w                  (make-world :current-time 0)
        [new-world events] (scheduler/resolve-action w 0 {:action/type :world/move :delta [1 0]})]
    (is (= [2 1] (:pos (:player new-world))))
    (is (= [{:event/type :world/moved}] events))
    (is (= 10 (:next-time (:player new-world))))))

(deftest resolve-action-move-blocked-still-reschedules
  (let [w                  (make-world :player (make-player {:pos [1 1]}) :current-time 0)
        [new-world events] (scheduler/resolve-action w 0 {:action/type :world/move :delta [-1 0]})]
    (is (= [1 1] (:pos (:player new-world))) "actor did not move, blocked by wall")
    (is (= [{:event/type :world/blocked :by :wall}] events))
    (is (= 10 (:next-time (:player new-world))) "cost is still applied even though the move failed")))

(deftest resolve-action-unknown-action-throws
  (let [w (make-world)]
    (is (thrown? clojure.lang.ExceptionInfo (scheduler/resolve-action w 0 {:action/type :bogus})))))

;;; next-scheduled

(deftest next-scheduled-picks-lowest-time-actor
  (let [player  (make-player {:next-time 20})
        monster (make-monster 1 {:next-time 5})
        w       (make-world :player player :monsters [monster] :next-tick 100)
        result  (scheduler/next-scheduled w)]
    (is (= :actor (:kind result)))
    (is (= 1 (:entity/id result)))
    (is (= 5 (:at result)))))

(deftest next-scheduled-tick-wins-when-lowest
  (let [player  (make-player {:next-time 50})
        monster (make-monster 1 {:next-time 60})
        w       (make-world :player player :monsters [monster] :next-tick 10)
        result  (scheduler/next-scheduled w)]
    (is (= :tick (:kind result)))
    (is (= 10 (:at result)))))

(deftest next-scheduled-actor-beats-tick-on-tie
  (let [player (make-player {:next-time 10})
        w      (make-world :player player :next-tick 10)
        result (scheduler/next-scheduled w)]
    (is (= :actor (:kind result)))
    (is (= 0 (:entity/id result)))))

(deftest next-scheduled-lowest-id-wins-among-actors-on-tie
  (let [player  (make-player {:next-time 10})
        monster (make-monster 1 {:next-time 10})
        w       (make-world :player player :monsters [monster] :next-tick 100)
        result  (scheduler/next-scheduled w)]
    (is (= :actor (:kind result)))
    (is (= 0 (:entity/id result)) "player (id 0) wins ties over monster")))

(deftest next-scheduled-multiple-monsters-lowest-id-wins-on-tie
  (let [player (make-player {:next-time 100})
        m1     (make-monster 2 {:next-time 5})
        m2     (make-monster 1 {:next-time 5})
        w      (make-world :player player :monsters [m1 m2] :next-tick 100)
        result (scheduler/next-scheduled w)]
    (is (= 1 (:entity/id result)))))

;;; tick-world

(deftest tick-world-advances-next-tick-by-10
  (let [w      (make-world :current-time 30)
        result (scheduler/tick-world w)]
    (is (= 40 (:next-tick result)))))

;;; set-world-time

(deftest set-world-time-sets-current-time
  (let [w      (make-world :current-time 0)
        result (scheduler/set-world-time w 77)]
    (is (= 77 (:current-time result)))))

;;; advance

(deftest advance-tick-kind-ticks-world-and-returns-ticked-status
  (let [player                       (make-player {:next-time 100})
        w                            (make-world :player player :next-tick 10 :current-time 0)
        [new-world events status]    (scheduler/advance w)]
    (is (= :ticked status))
    (is (= [] events))
    (is (= 10 (:current-time new-world)))
    (is (= 20 (:next-tick new-world)))))

(deftest advance-player-turn-returns-awaiting-input
  (let [player                     (make-player {:next-time 5})
        w                          (make-world :player player :next-tick 100 :current-time 0)
        [new-world events status]  (scheduler/advance w)]
    (is (= :awaiting-input status))
    (is (= [] events))
    (is (= 5 (:current-time new-world)) "world time advanced to the player's scheduled turn")
    (is (= player (:player new-world)) "player entity itself is untouched, awaiting input")))

(deftest advance-monster-turn-resolves-wait-via-ai
  (let [player  (make-player {:next-time 1000})
        monster (make-monster 1 {:next-time 5 :pos [1 1]})
        w       (make-world :player player :monsters [monster] :next-tick 1000 :current-time 0)]
    (with-redefs [ai/decide (fn [rng-state _ _] [rng-state {:action/type :world/wait}])]
      (let [result (scheduler/advance w)]
        (let [[new-world events status] result]
          (is (= :acted status))
          (is (= [] events))
          (is (= 15 (:next-time (level/get-entity (:current-level new-world) 1)))
              "monster rescheduled at current-time (5) + wait cost (10)"))))))

(deftest advance-monster-turn-moves-monster-when-ai-decides-move
  (let [player  (make-player {:next-time 1000})
        monster (make-monster 1 {:next-time 5 :pos [1 1]})
        w       (make-world :player player :monsters [monster] :next-tick 1000 :current-time 0)]
    (with-redefs [ai/decide (fn [rng-state _ _] [rng-state {:action/type :world/move :delta [1 0]}])]
      (let [[new-world events] (scheduler/advance w)]
        (is (= [{:event/type :world/moved}] events))
        (is (= [2 1] (:pos (level/get-entity (:current-level new-world) 1))))))))

(deftest advance-monster-turn-carries-updated-rng-state-into-world
  (let [player        (make-player {:next-time 1000})
        monster       (make-monster 1 {:next-time 5 :pos [1 1]})
        w             (make-world :player player :monsters [monster] :next-tick 1000 :current-time 0)
        sentinel-state [9 9 9 9]]
    (with-redefs [ai/decide (fn [_ _ _] [sentinel-state {:action/type :world/wait}])]
      (let [[new-world _] (scheduler/advance w)]
        (is (= sentinel-state (:rng-state new-world)))))))
