(ns roguelike.placement
  (:require [roguelike.rng :as rng]))

(defn scatter
  "Given a context, list of regions, an integer n, and two functions: one that
   breaks the region into positions (positions-of) and another that determines
   what positions are valid (valid?), this function chooses a random list of
   acceptable positions. It 'scatters' some object into the provided regions."
  [ctx regions n positions-of valid?]
  (let [[ctx chosen] (rng/draw-distinct ctx (min n (count regions)) regions)]
    (reduce (fn [[ctx acc] region]
              (let [candidates (filterv valid? (positions-of region))]
                (if (empty? candidates)
                  [ctx acc]
                  (let [[ctx pos] (rng/draw-any ctx candidates)]
                    [ctx (conj acc pos)]))))
            [ctx []]
            chosen)))
