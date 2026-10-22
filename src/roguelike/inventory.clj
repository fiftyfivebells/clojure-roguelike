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
