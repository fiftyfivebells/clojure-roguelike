(ns roguelike.monster.catalog)

;; TODO: if I ever need to add some default behavior per genus, thinkg about
;; adding a keyword :attrs that wraps :monster/flags and whatever the other
;; behavior might be
(def ^:private orcs
  {:monster/flags #{:opens-doors}

   :species {:orc/goblin {:depth 1
                          :group/size [3 5]
                          :sight/radius 6
                          :hp 8
                          :hp-variance 0.1
                          :to-hit 100
                          :armor-class 4
                          :attacks [{:method :swing :effect :physical :dice 1 :sides 8}]}

             :orc/hill-orc {:depth 3
                            :group/size [3 5]
                            :sight/radius 6
                            :hp 18
                            :hp-variance 0.1
                            :to-hit 100
                            :armor-class 8
                            :attacks [{:method :swing :effect :physical :dice 1 :sides 10}]}

             :orc/cave-orc {:depth 5
                            :group/size [3 5]
                            :sight/radius 6
                            :hp 30
                            :hp-variance 0.1
                            :to-hit 125
                            :armor-class 12
                            :attacks [{:method :swing :effect :physical :dice 2 :sides 8}
                                      {:method :swing :effect :physical :dice 2 :sides 8}]}}})

(def ^:private worm-masses
  {:monster/flags #{:breeds}

   :species {:worm-mass/white {:depth 0
                               :group/size [1 1]
                               :sight/radius 2
                               :hp 6
                               :hp-variance 0.1
                               :to-hit 70
                               :armor-class 1
                               :attacks [{:method :wriggle :effect :physical :dice 1 :sides 4}]}}})

(def ^:private canines
  {:monster/flags #{:animal}

   :species {:canine/wild-dog {:depth 0
                               :group/size [4 7]
                               :sight/radius 8
                               :hp 6
                               :hp-variance 0.2
                               :to-hit 90
                               :armor-class 1
                               :attacks [{:method :bite :effect :physical :dice 1 :sides 2}]}}})

(def genera
  {:orc       orcs
   :worm-mass worm-masses
   :canine    canines})
