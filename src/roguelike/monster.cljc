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
