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

(defn species-at-depth
  "Species that have started appearing by the given depth, as level ids.
   TODO: this currently just takes any monster whose :depth is <= the current
   level depth. This is OK for now but not robust enough for a real solution."
  [depth]
  (into [] (comp (filter (fn [[_ sp]] (<= (:depth sp) depth)))
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
