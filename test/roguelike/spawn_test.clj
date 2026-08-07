(ns roguelike.spawn-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.level :as level]
            [roguelike.monster :as monster]
            [roguelike.rng :as rng]
            [roguelike.spawn :as spawn]))

;;; Helpers

(def ^:private seed 123456789)

(defn- make-level
  "Solid rock with the given rooms carved out as floor, matching the
   {:bounds [x y w h]} shape dungeon generation produces. The rooms are left
   unconnected so that a group's spread stays checkable against room bounds."
  [rooms]
  (reduce (fn [lvl [rx ry rw rh :as bounds]]
            (-> (reduce #(level/set-tile %1 %2 (level/floor))
                        lvl
                        (for [x (range rx (+ rx rw))
                              y (range ry (+ ry rh))]
                          [x y]))
                (level/set-rooms (conj (or (level/get-rooms lvl) [])
                                       {:bounds bounds :lit? false}))))
          (level/solid-level)
          rooms))

(defn- spawn-on
  "The monsters alone, for tests that don't care about the advanced rng."
  ([lvl] (spawn-on lvl 0))
  ([lvl depth] (second (spawn/monsters-for-level seed depth lvl #{}))))

;; Group count is a fraction of the room count, so a fixture needs enough rooms
;; to get more than one group.
(def ^:private room-grid
  [[2 2 6 6] [12 2 6 6] [22 2 6 6] [32 2 6 6]
   [2 12 6 6] [12 12 6 6] [22 12 6 6] [32 12 6 6]])

;;; Determinism

(deftest same-seed-and-depth-produce-identical-monsters
  (let [lvl (make-level room-grid)]
    ;; comparing the whole return value also pins the advanced rng, not just the
    ;; monsters it produced
    (is (= (spawn/monsters-for-level seed 0 lvl #{})
           (spawn/monsters-for-level seed 0 lvl #{})))))

;;; Rng handoff

(deftest the-spawn-rng-is-returned-advanced
  (let [lvl (make-level room-grid)
        [ctx monsters] (spawn/monsters-for-level seed 0 lvl #{})]
    (is (seq monsters))
    (is (some? (:rng-state ctx)))
    (is (not= (:rng-state ctx) (rng/make (rng/mix seed :spawn 0)))
        "the returned state has moved past the seed it started from")))

(deftest the-rng-is-returned-even-when-nothing-spawns
  (with-redefs [monster/species-at-depth (constantly [])]
    (let [[ctx monsters] (spawn/monsters-for-level seed 0 (make-level room-grid) #{})]
      (is (empty? monsters))
      (is (some? (:rng-state ctx))
          "a caller still needs a stream to hand to later waves"))))

(deftest continuing-the-returned-rng-does-not-repeat-a-wave
  (let [lvl (make-level room-grid)
        [ctx monsters] (spawn/monsters-for-level seed 0 lvl #{})
        ;; what a reinforcement wave would do: keep drawing from where we stopped
        [_ next-draw] (rng/draw-int ctx 0 1000000)
        [_ same-draw] (rng/draw-int {:rng-state (rng/make (rng/mix seed :spawn 0))} 0 1000000)]
    (is (seq monsters))
    (is (not= next-draw same-draw)
        "re-deriving the seed would replay the stream from the start")))

(deftest different-depths-produce-different-monsters
  (let [lvl (make-level room-grid)]
    (is (not= (spawn-on lvl 0) (spawn-on lvl 4)))))

;;; Positions

(deftest every-monster-lands-on-a-walkable-tile
  (let [lvl (make-level room-grid)
        monsters (spawn-on lvl)]
    (is (seq monsters))
    (is (every? #(level/walkable-at? lvl (:pos %)) monsters))))

(deftest no-two-monsters-share-a-tile
  (let [lvl (make-level room-grid)
        monsters (spawn-on lvl)]
    (is (= (count monsters)
           (count (distinct (map :pos monsters)))))))

(deftest reserved-positions-are-left-empty
  (let [lvl (make-level room-grid)
        taken (set (map :pos (spawn-on lvl)))
        reserved (set (take 3 taken))
        monsters (second (spawn/monsters-for-level seed 0 lvl reserved))]
    (is (seq monsters))
    (is (empty? (filter reserved (map :pos monsters))))))

(deftest monsters-avoid-tiles-that-already-hold-an-entity
  (let [bare (make-level room-grid)
        occupied-pos (:pos (first (spawn-on bare)))
        lvl (level/add-entity bare {:entity/id 99 :pos occupied-pos})
        monsters (second (spawn/monsters-for-level seed 0 lvl #{}))]
    (is (empty? (filter #(= occupied-pos (:pos %)) monsters)))))

(deftest a-level-without-rooms-spawns-nothing
  (is (empty? (spawn-on (make-level [])))))

;;; Depth gating

(defn- species-across-seeds
  "The set of species spawned at the given depth over a spread of world seeds.
   A single seed only draws a couple of groups, so one level is too small a
   sample to say a species never appears."
  [depth]
  (let [lvl (make-level room-grid)]
    (into #{}
          (mapcat (fn [s]
                    (map :monster/species
                         (second (spawn/monsters-for-level s depth lvl #{})))))
          (range 40))))

(deftest species-below-their-depth-do-not-appear
  (testing "goblins are native to depth 1, so depth 0 never has them"
    (is (not (contains? (species-across-seeds 0) :orc/goblin))))
  (testing "they do appear once the player is deep enough"
    (is (contains? (species-across-seeds 1) :orc/goblin))))

(deftest only-eligible-species-are-spawned
  (doseq [depth (range 3)]
    (is (every? (set (monster/species-at-depth depth))
                (species-across-seeds depth)))))

(deftest nothing-spawns-when-no-species-is-eligible
  (with-redefs [monster/species-at-depth (constantly [])]
    (is (empty? (spawn-on (make-level room-grid))))))

;;; Groups

(defn- room-of
  "Index of the room containing pos, or nil. Groups are not tagged on the
   entity, so the room a monster sits in is what identifies its group."
  [[x y]]
  (some (fn [[i [rx ry rw rh]]]
          (when (and (<= rx x (+ rx rw -1))
                     (<= ry y (+ ry rh -1)))
            i))
        (map-indexed vector room-grid)))

(deftest groups-cluster-inside-one-room
  (with-redefs [monster/group-size (constantly [5 5])]
    (let [lvl (make-level room-grid)
          monsters (second (spawn/monsters-for-level seed 0 lvl #{}))
          by-room (group-by (comp room-of :pos) monsters)]
      (is (seq monsters))
      (is (= (count monsters) (count (distinct (map :pos monsters))))
          "a group must not stack monsters on one tile")
      (is (nil? (get by-room nil))
          "no monster escapes its room")
      (is (every? #(= 5 %) (map count (vals by-room)))
          "each group stays whole and inside a single room")
      (is (zero? (mod (count monsters) 5))
          "every group is placed at full strength"))))

(deftest group-count-scales-with-room-count
  (with-redefs [monster/group-size (constantly [1 1])]
    (let [groups-for (fn [n-rooms]
                       (count (spawn-on (make-level (take n-rooms room-grid)))))]
      (is (< (groups-for 2) (groups-for 8))
          "a level with more rooms gets more groups"))))

;;; Entity shape

(deftest spawned-entities-carry-no-world-owned-fields
  (let [monsters (spawn-on (make-level room-grid))]
    (is (every? #(nil? (:entity/id %)) monsters))
    (is (every? #(nil? (:next-time %)) monsters))))

(deftest spawn-only-catalog-keys-are-stripped
  (let [monsters (spawn-on (make-level room-grid))]
    (is (every? #(nil? (:depth %)) monsters))
    (is (every? #(nil? (:group/size %)) monsters))))

(deftest spawned-entities-are-monsters-with-genus-and-species
  (let [monsters (spawn-on (make-level room-grid))]
    (is (every? #(= :monster (:entity/type %)) monsters))
    (is (every? #(qualified-keyword? (:monster/species %)) monsters))
    (is (every? #(keyword? (:monster/genus %)) monsters))
    (is (every? #(number? (:sight/radius %)) monsters))))
