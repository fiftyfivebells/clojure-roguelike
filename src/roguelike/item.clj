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

;; :item/rarity is important for spawning, but it has no bearing on the actual
;; instance of an item, so it should be pulled out of item instances
(def ^:private spawn-only [:item/rarity])

(defn template
  [item-kind]
  (get resolved item-kind))

(defn items-at-depth
  "Items that have started appearing by the given depth, as level ids.
  Currently this should allow any item from the current depth and higher."
  [depth]
  (filter #(<= (:depth %) depth) resolved))

;; TODO: someday there might be items that are not spawnable, so there will
;; someday be a concept of a spawnable item vs. non-spawnable item. This
;; function will enumerate the items that can spawn in the dungeon
;; (def ^:private spawnable-categories
;;   #{:potion :scroll})

(defn instantiate
  "Builds a new item and adds it to the provided level id at the provided pos.
  The caller owns the :item/id property."
  [item-kind level-id pos]
  (-> (apply dissoc (template item-kind) spawn-only)
      (assoc :entity/type :item :level/id level-id :level/pos pos)))
