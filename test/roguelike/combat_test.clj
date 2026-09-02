(ns roguelike.combat-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.combat :as combat]
            [roguelike.rng :as rng]))

;;; Helpers

(def ^:private seed 42)

(defn- ctx-for
  [seed]
  {:rng-state (rng/make seed)})

(defn- make-attacker
  [& {:keys [to-hit attacks]
      :or   {to-hit 100
             attacks [{:method :swing :effect :physical :dice 1 :sides 6}]}}]
  {:to-hit to-hit :attacks attacks})

(defn- make-defender
  [& {:keys [armor-class evasion hp]
      :or   {armor-class 0 evasion 0 hp 100}}]
  {:armor-class armor-class :evasion evasion :hp/current hp})

;;; Determinism

(deftest resolve-attacks-is-deterministic
  (testing "identical starting ctx and profiles produce identical outcomes"
    (let [attacker (make-attacker :attacks [{:method :swing :effect :physical :dice 2 :sides 6}
                                             {:method :bite :effect :physical :dice 1 :sides 4}])
          defender (make-defender)
          [ctx-a outcomes-a] (combat/resolve-attacks (ctx-for seed) attacker defender)
          [ctx-b outcomes-b] (combat/resolve-attacks (ctx-for seed) attacker defender)]
      (is (= outcomes-a outcomes-b))
      (is (= ctx-a ctx-b)))))

;;; Ctx advances and is returned

(deftest resolve-attacks-advances-ctx-when-a-roll-occurs
  (testing "the returned ctx differs from the input whenever a roll happened"
    (let [attacker (make-attacker)
          defender (make-defender)
          starting-ctx (ctx-for seed)
          [ctx outcomes] (combat/resolve-attacks starting-ctx attacker defender)]
      (is (seq outcomes))
      (is (not= starting-ctx ctx)))))

(deftest threading-ctx-through-two-attacks-does-not-repeat
  (testing "resolving a second attack from the advanced ctx differs from resolving it fresh"
    (let [attacker (make-attacker)
          defender (make-defender)
          starting-ctx (ctx-for seed)
          [ctx-1 _] (combat/resolve-attacks starting-ctx attacker defender)
          [_ outcomes-2] (combat/resolve-attacks ctx-1 attacker defender)
          [_ outcomes-fresh] (combat/resolve-attacks starting-ctx attacker defender)]
      (is (not= outcomes-2 outcomes-fresh)
          "a dropped ctx would make the second attack replay the first"))))

;;; Per-blow independence

(defn- damage-values-across-seeds
  "Damage values from each of three identical blows, tried over a spread of
   seeds. A single seed could coincidentally roll the same damage on every
   blow, so this checks several seeds and only requires variation to show up
   in at least one, keeping the test from being flaky on an unlucky seed."
  [seeds]
  (let [attacker (make-attacker :attacks (repeat 3 {:method :claw :effect :physical
                                                     :dice 3 :sides 8}))
        defender (make-defender :hp 10000)]
    (map (fn [s]
           (let [[_ outcomes] (combat/resolve-attacks (ctx-for s) attacker defender)]
             (map :damage outcomes)))
         seeds)))

(deftest blows-from-the-same-attack-roll-independently
  (testing "damage across the three identical blows is not always identical"
    (let [results (damage-values-across-seeds (range 1 30))]
      (is (some #(apply distinct? %) results)))))

;;; Fatal short-circuit

(deftest a-fatal-blow-ends-the-outcome-vector
  (testing "no outcomes are produced for blows after a fatal one"
    (let [attacker (make-attacker :attacks (repeat 4 {:method :smash :effect :physical
                                                       :dice 4 :sides 8}))
          defender (make-defender :hp 1)
          [_ outcomes] (combat/resolve-attacks (ctx-for seed) attacker defender)]
      (is (= 1 (count outcomes)))
      (is (true? (:fatal? (last outcomes)))))))

;;; Misses do not short-circuit

(deftest a-miss-does-not-end-the-outcome-vector
  (testing "misses are followed by outcomes for the remaining blows"
    (let [attacker (make-attacker :to-hit 1
                                  :attacks (repeat 4 {:method :swing :effect :physical
                                                       :dice 1 :sides 4}))
          ;; extreme evasion drives the hit chance toward zero
          defender (make-defender :evasion 1000)
          [_ outcomes] (combat/resolve-attacks (ctx-for seed) attacker defender)]
      (is (= 4 (count outcomes)))
      (is (every? #(false? (:hit? %)) outcomes)))))

;;; Outcome shape

(deftest hit-outcomes-carry-the-full-key-set
  (testing "a hit outcome has method, effect, hit?, rolled, damage, resisted?, fatal?"
    (let [attacker (make-attacker)
          defender (make-defender :hp 10000)
          [_ [outcome]] (combat/resolve-attacks (ctx-for seed) attacker defender)]
      (is (true? (:hit? outcome)))
      (is (= #{:method :effect :hit? :rolled :damage :resisted? :fatal?}
             (set (keys outcome)))))))

(deftest miss-outcomes-carry-only-the-miss-key-set
  (testing "a miss outcome has method, effect, hit? and nothing else"
    (let [attacker (make-attacker :to-hit 1)
          defender (make-defender :evasion 1000)
          [_ [outcome]] (combat/resolve-attacks (ctx-for seed) attacker defender)]
      (is (false? (:hit? outcome)))
      (is (= #{:method :effect :hit?} (set (keys outcome)))))))

;;; Zero-damage hits are hits

(deftest a-zero-damage-hit-is-still-a-hit
  (testing "armor reducing damage to zero does not turn a hit into a miss"
    (let [attacker (make-attacker :attacks (repeat 20 {:method :poke :effect :physical
                                                        :dice 1 :sides 2}))
          defender (make-defender :armor-class 100 :hp 100000)]
      (is (some (fn [s]
                  (let [[_ outcomes] (combat/resolve-attacks (ctx-for s) attacker defender)]
                    (some #(and (:hit? %) (zero? (:damage %))) outcomes)))
                (range 1 30))))))

;;; Damage is never negative

(deftest damage-is-never-negative
  (testing "high armor against low damage dice never produces negative damage"
    (let [attacker (make-attacker :attacks (repeat 20 {:method :poke :effect :physical
                                                        :dice 1 :sides 2}))
          defender (make-defender :armor-class 100 :hp 100000)]
      (doseq [s (range 1 30)]
        (let [[_ outcomes] (combat/resolve-attacks (ctx-for s) attacker defender)]
          (is (every? #(>= (:damage %) 0) outcomes)))))))

;;; did-hit?

(deftest did-hit-always-hits-with-full-accuracy-and-no-evasion
  (testing "to-hit 100 and evasion 0 always hits"
    (doseq [s (range 1 100)]
      (let [[_ hit?] (#'combat/did-hit? (ctx-for s)
                      {} {:to-hit 100} {:evasion 0})]
        (is (true? hit?))))))

(deftest did-hit-is-rare-but-not-impossible-with-extreme-evasion
  (testing "the observed hit rate over many draws is in a loose band around the analytic value"
    ;; hit-chance = 100 * 0.987^400 ≈ 0.514%, so over 1000 draws we expect
    ;; roughly 5 hits; a wide [1, 30] band keeps this from flaking
    (let [n 1000
          hits (loop [ctx (ctx-for seed) i 0 hits 0]
                 (if (= i n)
                   hits
                   (let [[ctx' hit?] (#'combat/did-hit? ctx
                                       {} {:to-hit 100} {:evasion 400})]
                     (recur ctx' (inc i) (if hit? (inc hits) hits)))))]
      (is (<= 1 hits 30) (str "observed " hits " hits out of " n)))))

;;; calculate-physical-mitigation

(deftest zero-armor-never-reduces-damage
  (testing "armor-class 0 always draws a zero reduction, so damage passes through unchanged"
    ;; the draw is (draw-int ctx 0 (inc armor-class)); getting the inc wrong
    ;; would make this throw for unarmored defenders
    (doseq [s (range 1 30)]
      (let [[_ damage] (#'combat/calculate-physical-mitigation (ctx-for s) 0 10)]
        (is (= 10 damage))))))

(deftest physical-mitigation-never-exceeds-armor-class
  (testing "reduction is drawn from [0, armor-class] inclusive"
    (doseq [s (range 1 30)]
      (let [[_ damage] (#'combat/calculate-physical-mitigation (ctx-for s) 10 10)]
        (is (>= damage 0))))))

(deftest physical-mitigation-floors-at-zero
  (testing "reduction larger than the rolled damage floors the result at zero, never negative"
    (doseq [s (range 1 30)]
      (let [[_ damage] (#'combat/calculate-physical-mitigation (ctx-for s) 1000 5)]
        (is (>= damage 0))))))

;;; mitigate-damage

(deftest physical-mitigation-never-exceeds-rolled-damage
  (testing "the mitigated :damage is at most the :rolled damage"
    (doseq [s (range 1 30)]
      (let [[_ result] (#'combat/mitigate-damage (ctx-for s)
                        {:effect :physical :rolled 20} {:armor-class 10})]
        (is (<= (:damage result) (:rolled result)))))))

(deftest non-physical-effects-bypass-armor
  (testing "a cold attack ignores armor-class entirely, even when it's high"
    (let [[_ result] (#'combat/mitigate-damage (ctx-for seed)
                      {:effect :cold :rolled 20} {:armor-class 1000})]
      (is (= 20 (:damage result)))
      (is (false? (:resisted? result))))))

(deftest mitigate-damage-returns-a-two-vector-on-both-branches
  (testing "physical and non-physical branches both return [ctx result]"
    (let [physical (#'combat/mitigate-damage (ctx-for seed)
                    {:effect :physical :rolled 10} {:armor-class 5})
          elemental (#'combat/mitigate-damage (ctx-for seed)
                     {:effect :cold :rolled 10} {:armor-class 5})]
      (is (= 2 (count physical)))
      (is (some? (first physical)))
      (is (= 2 (count elemental)))
      (is (some? (first elemental))))))

;;; roll-damage

(deftest roll-damage-stays-within-the-dice-range-and-carries-effect
  (testing "result is within [dice, dice*sides] and :effect passes through"
    (doseq [s (range 1 30)]
      (let [[_ result] (#'combat/roll-damage (ctx-for s)
                        {:effect :fire :dice 3 :sides 6})]
        (is (<= 3 (:rolled result) 18))
        (is (= :fire (:effect result)))))))

;;; attacker-profile / defender-profile

(deftest attacker-profile-selects-only-to-hit-and-attacks
  (testing "a monster entity's profile carries only :to-hit and :attacks"
    (let [monster {:entity/type :monster :to-hit 90 :attacks [{:method :bite}]
                    :hp/current 5 :pos [0 0]}
          profile (combat/attacker-profile monster)]
      (is (= #{:to-hit :attacks} (set (keys profile)))))))

(deftest attacker-profile-routes-player-through-player-attack-profile
  (testing "a player entity produces the same key set as a monster's profile"
    (let [player {:entity/type :player :to-hit 80 :attacks [{:method :swing}]
                  :hp/current 20 :pos [0 0]}
          profile (combat/attacker-profile player)]
      (is (= #{:to-hit :attacks} (set (keys profile)))))))

(deftest defender-profile-defaults-armor-class-and-evasion-to-zero
  (testing "an entity with neither key gets 0 for both, as most catalog species do"
    (let [entity {:entity/type :monster :hp/current 10}
          profile (combat/defender-profile entity)]
      (is (= 0 (:armor-class profile)))
      (is (= 0 (:evasion profile))))))

(deftest defender-profile-keeps-explicit-armor-class-and-evasion
  (testing "explicit values are not overridden by the defaults"
    (let [entity {:entity/type :monster :armor-class 5 :evasion 3 :hp/current 10}
          profile (combat/defender-profile entity)]
      (is (= 5 (:armor-class profile)))
      (is (= 3 (:evasion profile))))))
