(ns roguelike.ui.appearance.monster)

(def ^:private orcs
  {:glyph   "o"
   :species {:orc/goblin   {:name "goblin"   :color :green}
             :orc/hill-orc {:name "hill orc" :color :brown}}})

(def ^:private worm-masses
  {:glyph   "w"
   :species {:worm-mass/white {:name "white worm mass" :color :white}}})

(def ^:private canines
  {:glyph   "C"
   :species {:canine/wild-dog {:name "wild dog" :color :brown}}})

(def ^:private genus->appearance
  {:canine canines
   :orc orcs
   :worm-mass worm-masses})

(def ^:private species->appearance
  (into {} (mapcat :species) (vals genus->appearance)))

(defn name-of
  [species]
  (:name (species->appearance species) :unknown))

(defn color-of
  [species]
  (:color (species->appearance species) :pink))

(defn glyph-of
  [genus]
  (:glyph (genus->appearance genus) "?"))
