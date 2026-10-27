(ns roguelike.ui.appearance.item 
  (:require
    [clojure.string :as str]))

(def ^:private edged
  {:glyph "|"
   :color :white
   :kinds {:edged/dagger    {:name "dagger"}
           :edged/longsword {:name "longsword"}}})

(def ^:private hafted
  {:glyph "\\"
   :color :white
   :kinds {:hafted/club {:name "club"}}})

(def ^:private polearm
  {:glyph "/"
   :color :white
   :kinds {:polearm/spear {:name "spear"}}})

(def ^:private potion-kinds
  {:potion/lesser-healing {:name "Lesser Healing"}
   :potion/harm           {:name "Harm"}})

(def ^:private potion-adjectives
  {:bubbling {:word "bubbling"}
   :frothy   {:word "frothy"}
   :glowing  {:word "glowing"}})

(def ^:private potion-colors
  {:azure   {:word "azure"   :color :blue}
   :crimson {:word "crimson" :color :red}
   :purple  {:word "purple"  :color :purple}})

(def ^:private potion
  {:glyph      "!"
   :noun       "potion"
   :unidentified-format {:one "%s potion"    :many "%s potions"}
   :identified-format   {:one "Potion of %s" :many "Potions of %s"}
   :kinds      potion-kinds})

(def ^:private scroll-kinds
  {:scroll/phase-door {:name "Phase Door"}
   :scroll/teleport   {:name "Teleport"}})

(def ^:private scroll
  {:glyph "?"
   :noun  "scroll"
   :color :white
   :unidentified-format {:one "scroll titled %s" :many "scrolls titled %s"}
   :identified-format   {:one "Scroll of %s"     :many "Scrolls of %s"}
   :kinds scroll-kinds})

(def ^:private inheritable
  [:glyph :noun :color :unidentified-format :identified-format])

(defn- flatten-category
  "Flattens a category into normalized map with the keys that matter for
  appearance. Allows default keywords to be applied to evey item in a group,
  while still letting overrides take precedence."
  [[category node]]
  (let [defaults (assoc (select-keys node inheritable) :category category)]
    (into {} (map (fn [[k v]] [k (merge defaults v)])) (:kinds node))))

(def ^:private kind->appearance
  "Complete catalog of all items. Flattened/normalized so each entry looks
  something like this:

  {:glyph \"!\",
  :noun \"potion\",
  :unidentified-format {:one \"%s potion\" :many \"%s potions\"},
  :identified-format {:one \"Potion of %s\" :many \"Potions of %s\"},
  :category :potion,
  :name \"Lesser Healing\"}"

  (let [edgeds   (flatten-category [:edged edged])
        hafteds  (flatten-category [:hafted hafted])
        polearms (flatten-category [:polearm polearm])
        potions  (flatten-category [:potion potion])
        scrolls  (flatten-category [:scroll scroll])]
    (merge edgeds
           hafteds
           polearms
           potions
           scrolls)))

;; names come in two forms, picked by keyword: :one for a single item, :many for
;; a stack. Formats are maps like {:one "%s potion" :many "%s potions"}. Weapons
;; don't stack, so their :name is a plain string with no plural.

(defn- format-unidentified
  [appearance form words]
  (format (form (:unidentified-format appearance)) (str/join " " words)))

(defn- format-identified
  [appearance form name]
  (if-let [id-format (:identified-format appearance)]
    (format (form id-format) name)
    name))

(defn- potion-words
  [{:keys [adjective color]}]
  (map #(:word %) [(adjective potion-adjectives)
                   (color potion-colors)]))

(defn- scroll-words
  "Takes in a vector of vectors, where each of the nested vectors is a list of
  syllables. Maps over the syllable lists and creates a phrase of words to use
  as the name of the scroll. Makes sure to capitalize the words returned so it
  render as all caps.

  Example:
  [[:ka :zor] [:mul :gan :el]] comes in, 'KAZOR MULGANEL' is returned." 
  [{:keys [title]}]
  (let [render-syllables #(apply str (map name %))
        render-and-upper (comp str/upper-case render-syllables)]
    (map render-and-upper title)))

(defn- build-unidentified-name
  [appearance form flavor]
  (let [category (:category appearance)
        words (case category
                :potion (potion-words flavor)
                :scroll (scroll-words flavor)
                (throw
                 (ex-info "unknown item category" {:category category
                                                   :flavor   flavor})))]
    (format-unidentified appearance form words)))

;; flavors is a map of item/kind to it's "flavor". The flavor will be different
;; depending on the type of item. An example of the flavors map:
;;
;; {:potion/lesser-healing {:adjective :bubbling
;;                          :color     :azure}
;;  :potion/harm           {:adjective :glowing
;;                          :color     :crimson}
;;  :scroll/phase-door     {:title [[:ka :zor] [:en :mul]]}}

;; appearance
;;
;; {:glyph "!",
;;   :noun "potion",
;;   :unidentified-format {:one "%s potion" :many "%s potions"},
;;   :identified-format {:one "Potion of %s" :many "Potions of %s"},
;;   :category :potion,
;;   :name "Lesser Healing"}

(defn- item-name
  "The item's name in the given form (:one or :many)."
  [item flavors known form]
  (let [item-kind (:item/kind item)
        appearance (item-kind kind->appearance)]
    (when (nil? appearance)
      (throw (ex-info "given item kind has no appearance"
                      {:item/kind item-kind})))
    (let [flavor (item-kind flavors)]
      (if (or (contains? known item-kind)
              (nil? flavor))
        (format-identified appearance form (:name appearance))
        (build-unidentified-name appearance form flavor)))))

(defn display-name
  "Builds the singular display name for the item. For weapons, displays the
  :name field, but there is logic for potions and scrolls. Takes the flavors
  combos that the world rolled along with what the player knows. If the player
  already knows the item or there is no flavor for the item, just display the
  true name. Otherwise we show the 'unidentified' name."
  [item flavors known]
  (item-name item flavors known :one))

(defn describe
  "The item's name with its count, like Angband's item lists: 'a dagger',
  'an azure potion', '3 Potions of Harm', 'no more Potions of Harm'."
  [item flavors known]
  (let [n (:item/quantity item)]
    (cond
      (zero? n) (str "no more " (item-name item flavors known :many))
      (> n 1)   (str n " " (item-name item flavors known :many))
      :else
      (let [single (item-name item flavors known :one)]
        (if (#{\a \e \i \o \u} (Character/toLowerCase (first single)))
          (str "an " single)
          (str "a " single))))))

(defn color-of
  [item flavors]
  (let [item-kind (:item/kind item)
        appearance (item-kind kind->appearance)
        flavor (flavors item-kind)]
    (or (some-> (:color flavor)
                potion-colors
                :color)
        (:color appearance))))

(defn glyph-of
  [item-kind]
  (:glyph (item-kind kind->appearance)))

(defn cycler
  [n]
  (loop [n n
         nums (cycle [1 2 3])
         r []]
    (if (zero? n)
      r
      (recur (dec n) (rest nums) (conj r (first nums))))))
