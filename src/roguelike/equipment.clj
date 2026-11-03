(ns roguelike.equipment)

;; equipment is a map of slot -> item. Empty slots are absent.
;; slots: :main-hand, :off-hand, :body
;;
;; weapons have :item/hands (1 or 2) and go in the hands. A two-handed weapon
;; always sits in :main-hand and leaves :off-hand empty. Other gear names its
;; slot with :item/slot (ex. armor has :item/slot :body).

;; TODO: this is fine for now, but eventually I want different to-hit penalties
;; for different weapons, ie. a dagger has a smaller off-hand to-hit penalty as
;; compared to a longsword. Maybe I'll calculate the to-hit penalty off of the
;; weapon's weight?
(def ^:private off-hand-penalty
  "To-hit modifier on the off-hand weapon's attack."
  -25)

(defn- two-handed?
  [item]
  (= 2 (:item/hands item)))

(defn- weapon?
  [item]
  (contains? item :item/hands))

(defn slot-for
  "The slot an item goes into when equipped, or :ambiguous when it's a
  one-handed weapon and both hands already hold one-handed items (the player
  must choose)."
  [equipment item]
  (let [main (:main-hand equipment)
        off  (:off-hand equipment)]
    (cond
      (not (weapon? item)) (:item/slot item)
      (two-handed? item)   :main-hand
      (nil? main)          :main-hand
      (two-handed? main)   :main-hand
      (nil? off)           :off-hand
      :else                :ambiguous)))

(defn- displaced-slots
  "The slots that must be emptied to put item in slot."
  [equipment item slot]
  (cond
    (two-handed? item) [:main-hand :off-hand]
    (and (= slot :off-hand) (two-handed? (:main-hand equipment))) [:main-hand :off-hand]
    :else [slot]))

(defn equip
  "Puts item in slot. Returns [equipment displaced], where displaced is a
  vector of the items that had to come off to make room."
  [equipment item slot]
  (let [slots (displaced-slots equipment item slot)
        displaced (into [] (keep equipment) slots)
        new-equipment (-> (apply dissoc equipment slots)
                          (assoc slot item))]
    [new-equipment displaced]))

(defn unequip
  "Empties slot. Returns [equipment item], with item nil if the slot was empty."
  [equipment slot]
  [(dissoc equipment slot) (get equipment slot)])

(defn- weapon-attack
  [weapon]
  (let [{:keys [dice sides]} (:item/damage weapon)]
    {:method (:item/method weapon) :effect :physical :dice dice :sides sides}))

(defn weapon-attacks
  "One attack per wielded weapon, main hand first. The off-hand attack carries
  a :to-hit-mod penalty. Empty when no weapon is wielded."
  [equipment]
  (let [main (:main-hand equipment)
        off  (:off-hand equipment)]
    (cond-> []
      main (conj (weapon-attack main))
      off  (conj (assoc (weapon-attack off) :to-hit-mod off-hand-penalty)))))

(defn armor-class
  "Sum of :item/armor-class across everything equipped."
  [equipment]
  (reduce + 0 (keep :item/armor-class (vals equipment))))
