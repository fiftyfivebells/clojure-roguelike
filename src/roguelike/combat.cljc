(ns roguelike.combat 
  (:require
   [#?(:clj clojure.math
       :cljs   cljs.math) :as math]
    [roguelike.rng :as rng]))

;; attacker-profile keys:
;; :to-hit  -> number representing a chance to hit
;; :attack  -> map of keys describing the attack
;;             ex. {:method :swing :effect :physical :dice 2 :sides 4}
;; :attacks -> vector of attacks.

;; attacker-profile example:
;; this is an attacker that gets two swings with a 2d4 weapon
;; {:to-hit 100
;;  :attacks [{:method :touch :effect :cold :dice 1 :sides 4}
;;            {:method :swing :effect :physical :dice 2 :sides 4}]}

;; resolved-attack:
;; {:effect -> damage type
;;  :rolled -> total value rolled for damage}

;; mitigated-attack:
;; {:effect -> damage type
;;  :rolled -> total value rolled for damage
;;  :damage -> actual damage done after mitigation
;;  :resisted? -> was the rolled damage reduced?}

;; final returned attack
;; hit:
;; {:method -> flavor text: the type of blow
;;  :effect -> damage type
;;  :hit?   -> did the hit land on the target?
;;  :rolled -> damage rolled 
;;  :damage -> actual damage done after mitigation
;;  :resisted? -> was any damage absorbed?
;;  :fatal? -> did the damage kill the target?}

;; miss:
;; {:method -> flavor text
;;  :effect -> damage type
;;  :hit?   -> always false for misses}

;; defender-profile keys:
;; :armor-class -> armor class. This is pure damage reduction
;; :evasion     -> how hard the entity is to actually hit
;; :resistances -> maybe a map of different effects and a resistance score?
;;                 ie. {:cold 15 :fire 0 etc}
;;                 a map means I can assume nil == 0 resistance

(defn- player-attack-profile
  "Builds the player's attack profile, which needs to take items and stats into
  account. TODO: for now, this is identical to monsters, but it won't always
  be."
  [player]
  (select-keys player [:to-hit :attacks]))

(defn attacker-profile
  "Takes the relevant keys off the entity to build an 'attacker profile' for the
  combat functions to use to calculate damage."
  [attacker]
  (if (= (:entity/type attacker) :player)
    (player-attack-profile attacker)
    (select-keys attacker [:to-hit :attacks])))

(defn defender-profile
  "Takes the relevant keys off the entity to build a 'defender profile' for the
  combat functions to use to mitigate damage."
  [defender]
  (merge {:armor-class 0 :evasion 0}
         (select-keys defender [:armor-class :evasion :resistances :hp/current])))

(defn- roll-damage
  "Simply rolls the dice and gives back the result. This represents the maximum
  amount of damage that can be done by this particular attack. The result will
  be run through the mitigate-damage function to see if there should be any
  reductions."
  [ctx attack]
  (let [[ctx dmg] (rng/roll-dice ctx attack)]
    [ctx {:effect (:effect attack)
          :rolled dmg}]))

(defn- calculate-physical-mitigation
  [ctx armor-class damage]
  (let [[ctx reduction] (rng/draw-int ctx 0 (inc armor-class))]
    [ctx (max 0 (- damage reduction))]))

(defn- mitigate-damage
  "Takes in the resolved attack and then attempts to apply it to
  the defender's profile. Apply any relevant damage reductions here and then
  return the result."
  [ctx attack {:keys [armor-class resistances]}]
  (if (= (:effect attack) :physical)
    (let [[ctx damage] (calculate-physical-mitigation ctx armor-class (:rolled attack))]
      [ctx (merge attack {:damage damage :resisted? (< damage (:rolled attack))})])
    [ctx (merge attack {:damage (:rolled attack) :resisted? false})]))

(defn- did-hit?
  "TODO: implement me."
  [ctx attack {:keys [to-hit]} {:keys [evasion]}]  
  ;; the hit chance formula was adapted from brogue's approach
  (let [hit-chance (* to-hit (math/pow 0.987 evasion))
        [ctx attack-roll] (rng/draw-int ctx 0 100)]
    [ctx (< attack-roll hit-chance)]))

(defn- resolve-attack
  "Resolves a single attack. This function:
  1. calculates whether the attack was a hit.
  2a. if not, it returns immediately with a miss result.
  2b. if yes, it resolves the attack into damage using info on the attack.
  3. then it mitigates the damage with the attacker and defender profiles.
  4. finally, it calculates the new hp, checks if fatal, and returns the result."
  [ctx attack attacker-profile defender-profile current-hp]
  (let [[ctx hit?] (did-hit? ctx attack attacker-profile defender-profile)]
    (if-not hit?
      [ctx current-hp {:method (:method attack)
            :effect (:effect attack)
            :hit? false}]
      (let [[ctx resolved-atk] (roll-damage ctx attack)
            [ctx mitigated] (mitigate-damage ctx resolved-atk defender-profile)
            new-hp (- current-hp (:damage mitigated))
            fatal? (<= new-hp 0)]
        [ctx new-hp (merge mitigated
                    {:method (:method attack)
                     :hit? true
                     :fatal? fatal?})]))))

(defn resolve-attacks
  "Resolves a list of attacks. Iterates over a list of attacks and resolves each
  one individually. Returns a list of the resolution of each attempted attack.
  This function returns immediately when the list of attacks is empty, and it
  also returns early when an attack's result is a fatal blow."
  [ctx attacker-profile defender-profile]
  (let [hp (:hp/current defender-profile)]
    (loop [ctx ctx
           attacks (:attacks attacker-profile)
           current-hp hp
           acc []]
      (if (empty? attacks)
        [ctx acc]
        (let [attack       (first attacks)
              [ctx new-hp result] (resolve-attack ctx attack attacker-profile defender-profile current-hp)
              acc (conj acc result)]
          (if-not (:hit? result)
            (recur ctx (rest attacks) current-hp acc)
            (if (:fatal? result)
              [ctx acc]
              (recur ctx (rest attacks) new-hp acc))))))))
