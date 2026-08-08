(ns roguelike.monster
  (:require [roguelike.monster.catalog :as catalog]))

(defn- resolve-genus
  [[genus-key {:keys [species] :as genus}]]
  (let [inherited (dissoc genus :species)]
    (map (fn [[species-key sp]]
           [species-key (merge inherited sp
                               {:monster/genus genus-key
                                :monster/species species-key})])
         species)))

(def ^:private resolved (into {} (mapcat resolve-genus) catalog/genera))

;; :depth and :group/size drive spawning but are not live entity state, so they
;; get stripped when a template becomes an entity.
(def ^:private spawn-only [:depth :group/size])

(defn template
  [species]
  (get resolved species))

;; TODO: This is OK for now but might not robust enough for a real solution.
(defn species-at-depth
  "Species that have started appearing by the given depth, as level ids.
   Currently allows monsters from within 3 depths lower and 2 depths higher"
  [depth]
  (into [] (comp (filter (fn [[_ sp]] (<= (- depth 3) (:depth sp) (+ depth 2))))
                 (map key))
        resolved))

(defn group-size
  "The [min max] band size for species, inclusive on both ends."
  [species]
  (:group/size (template species)))

(defn instantiate
  "Builds a live monster entity at pos. The caller owns :entity/id and
   :next-time, so neither is set here."
  [species pos]
  (-> (apply dissoc (template species) spawn-only)
      (assoc :entity/type :monster :pos pos)))
