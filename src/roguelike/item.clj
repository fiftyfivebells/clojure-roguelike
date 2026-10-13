(ns roguelike.item
  (:require [roguelike.item.catalog :as catalog]))

(defn- resolve-category
  [[category-key category-map]]
  (let [inherited (dissoc category-map :kinds)]
    (map (fn [[item-kind stats]]
           [item-kind (merge inherited stats
                             {:item/category category-key
                              :item/kind item-kind})])
         (:kinds category-map))))

(def ^:private resolved
  (into {} (mapcat resolve-category) catalog/items))

;; :item/alloc is important for spawning, but it has no bearing on the actual
;; instance of an item, so it should be pulled out of item instances
(def ^:private spawn-only [:item/alloc])
(def max-depth 50)

(def flavored-items
  (-> (select-keys catalog/items [:potion :scroll])
      (update-vals #(vec (keys (:kinds %))))))

(defn template
  [item-kind]
  (get resolved item-kind))

(defn commonness
  [item-kind]
  (-> (template item-kind)
      :item/alloc
      :commonness))

(defn- eligible?
  "Checks whether the given item is eligible to spawn on the given depth. 
  Returns true if depth is >= the :from key and <= the :to key. :to is optional,
  so if :to doesn't exist it uses max-depth."
  [depth {{:keys [from to]} :item/alloc}]
  (<= from depth (or to max-depth)))

(defn items-at-depth
  "Items that are allowed to appear at the current depth. Filters down the list
  of all items to only those with a :from value of >= the current depth and a
  :to value of <= current depth (if :to is present)."
  [depth]
  (into [] (comp (filter (fn [[_ t]] (eligible? depth t)))
                 (map key))
        resolved))

;; TODO: someday there might be items that are not spawnable, so there will
;; someday be a concept of a spawnable item vs. non-spawnable item. This
;; function will enumerate the items that can spawn in the dungeon
;; (def ^:private spawnable-categories
;;   #{:potion :scroll})

(defn instantiate
  "Builds a new item and adds it to the provided level id at the provided pos.
  The caller owns the :item/id property."
  [item-kind]
  (-> (apply dissoc (template item-kind) spawn-only)
      (assoc :entity/type :item)))
