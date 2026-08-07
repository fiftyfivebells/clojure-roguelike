(ns roguelike.monster.catalog)

;; TODO: if I ever need to add some default behavior per genus, thinkg about
;; adding a keyword :attrs that wraps :monster/flags and whatever the other
;; behavior might be
(def ^:private orcs
  {:monster/flags #{:opens-doors}

   :species {:orc/goblin {:depth 1
                          :group/size [1 1]
                          :sight/radius 6
                          :hp 10}}})

(def ^:private worm-masses
  {:monster/flags #{:breeds}

   :species {:worm-mass/white {:depth 0
                               :group/size [1 1]
                               :sight/radius 2
                               :hp 5}}})

(def ^:private canines
  {:monster/flags #{:animal}

   :species {:canine/wild-dog {:depth 0
                               :group/size [1 8]
                               :sight/radius 8
                               :hp 8}}})

(def genera
  {:orc       orcs
   :worm-mass worm-masses
   :canine    canines})
