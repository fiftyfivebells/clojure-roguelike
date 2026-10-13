(ns roguelike.item.flavor
  (:require [roguelike.rng :as rng]))

(def ^:private potion-adjectives
  [:bubbling :frothy :glowing])

(def ^:private potion-colors
  [:azure :crimson :purple])

(def ^:private potion-pools
  {:adjectives potion-adjectives
   :colors     potion-colors})

(def ^:private scroll-syllables
  [:ka :zor :en :mul :phi :xa :dar :vel :quo :ith :bra :nok
   :sul :ep :or :gan :fli :tam :yr :ush :cor :wen :lex :dro
   :mi :za :og :pen :tha :riv :ul :jek :os :an :bel :kri])

(defn- make-word
  "Returns [holder word], where word is 2-3 distinct syllables joined together."
  [holder syllables]
  (let [[holder n] (rng/draw-int holder 2 4)
        [holder syls] (rng/draw-distinct holder n syllables)]
    [holder syls]))

(defn- make-title
  "Returns [holder title], where title is vector of 'words', which are vectors
  of syllable keywords."
  [holder syllables]
  (let [[holder n-words] (rng/draw-int holder 2 4)]
    (loop [holder holder
           i n-words
           words []]
      (if (zero? i)
        [holder words]
        (let [[holder word] (make-word holder syllables)]
          (recur holder (dec i) (conj words word)))))))

(defn- roll-scroll-flavors
  "Takes a holder with the shape {:rng-state ..., :flavors {}}, and returns a
  new holder with the rng-state updated and the flavors map filled with a unique
  title for each scroll kind. Titles are a vector of vectors of syllable
  keywords, like [[:ka :zor] [:gor :el]]. The flavor map will have entries like:
  {:scroll/phase-door {:title [[:ka :zor] [:gor :el]]}}"
  [holder scroll-kinds syllables]
  (loop [holder holder
         kinds scroll-kinds
         used #{}]
    (if-let [kind (first kinds)]
      (let [[holder title] (make-title holder syllables)]
        (if (used title)
          (recur holder kinds used)
          (recur (assoc-in holder [:flavors kind] {:title title})
                 (rest kinds)
                 (conj used title))))
      holder)))

(defn- roll-potion-flavors
  "Takes a holder with the shape {:rng-state ..., :flavors {}}, and returns a
  new holder with the rng-state updated and the flavors map filled with a unique
  adjective/color combo for every potion kind. The potion flavors are maps with
  a shape like: {:adjective :bubbling, :color :azure}."
  [holder potion-kinds {:keys [adjectives colors]}]
  (loop [holder holder
         kinds potion-kinds
         used #{}]
    (if-let [kind (first kinds)]
      (let [[holder adj] (rng/draw-any holder adjectives)
            [holder col] (rng/draw-any holder colors)
            combo #{adj col}]
        (if (contains? used combo)
          (recur holder kinds used)
          (recur (assoc-in holder [:flavors kind] {:adjective adj :color col})
                 (rest kinds)
                 (conj used combo))))
      holder)))

(defn roll-flavors
  [world-seed item-kinds]
  (let [ctx {:rng-state (rng/mix-and-make world-seed :flavors 0)
             :flavors {}}]
    (-> ctx
        (roll-scroll-flavors (:scroll item-kinds) scroll-syllables)
        (roll-potion-flavors (:potion item-kinds) potion-pools)
        :flavors)))
