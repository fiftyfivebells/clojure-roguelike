(ns roguelike.inventory)

(defn- stacks-with?
  "Two items stack when their kind is stackable and they're the same kind."
  [a b]
  (and (:item/stackable? a)
       (= (:item/kind a) (:item/kind b))))

(defn add-item
  "Adds an item to the inventory. If an existing stack can hold it, the
  item's quantity is merged into that stack; otherwise it's added as a new
  entry."
  [inventory item]
  (if-let [idx (first (keep-indexed (fn [i held] (when (stacks-with? held item) i))
                                    inventory))]
    (update-in inventory [idx :item/quantity] + (:item/quantity item))
    (conj inventory item)))

(defn add-items
  [inventory items]
  (reduce add-item inventory items))

(defn find-item-by-id
  [inventory item-id]
  (first (filter #(= item-id (:item/id %)) inventory)))

(defn find-item
  [inventory item]
  (first (filter #(= (:item/kind item) (:item/kind %)) inventory)))

(defn remove-one
  "Takes one item off the stack with the given id. A stack of one is removed
  from the inventory entirely."
  [inventory item-id]
  (into []
        (keep (fn [item]
                (cond
                  (not= item-id (:item/id item)) item
                  (> (:item/quantity item) 1) (update item :item/quantity dec)
                  :else nil)))
        inventory))

(defn remove-quantity
  "Takes n items off the stack with the given id. Taking all of them removes
  the stack from the inventory entirely."
  [inventory item-id n]
  (into []
        (keep (fn [item]
                (cond
                  (not= item-id (:item/id item)) item
                  (> (:item/quantity item) n) (update item :item/quantity - n)
                  :else nil)))
        inventory))

(defn remove-stack
  "Takes the entire stack with the given id and removes it from the inventory
  entirely."
  [inventory item-id]
  (vec (remove #(= (:item/id %) item-id) inventory)))

;; Order categories appear in the pack. Mirrors Angband's descending tval order
;; (list-tvals.h): potion, scroll, sword, polearm, hafted.
(def ^:private category-order
  [:potion :scroll :edged :polearm :hafted])

(def ^:private category-rank
  (zipmap category-order (range)))

(defn- sort-key
  "Mirrors Angband's earlier_object: category first, then unidentified
  flavored items last, then kind. An item is unidentified when its kind has a
  flavor and the player doesn't know it yet."
  [flavors known item]
  (let [{:item/keys [category kind]} item
        rank (category-rank category)
        unaware? (and (contains? flavors kind)
                      (not (contains? known kind)))]
    (when (nil? rank)
      (throw (ex-info "item category has no pack order" {:item/category category})))
    [rank (if unaware? 1 0) (name kind)]))

(defn pack
  "Returns the inventory in display order, each item paired with its letter:
  [{:label \\a :item {...}} ...]. Letters are positions in this sorted list, so
  they shift as items come and go. Never store them."
  [inventory flavors known]
  (map-indexed (fn [i item] {:label (char (+ (int \a) i)) :item item})
               (sort-by #(sort-key flavors known %) inventory)))
