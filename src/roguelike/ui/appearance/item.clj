(ns roguelike.ui.appearance.item 
  (:require
    [clojure.string :as str]
    [roguelike.ui.appearance :as appearance]))

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
   :unidentified-format "%s potion"
   :identified-format "Potion of %s"
   :kinds      potion-kinds})

(def ^:private scroll-kinds
  {:scroll/phase-door {:name "Phase Door"}})

(def ^:private scroll
  {:glyph "?"
   :noun  "scroll"
   :unidentified-format "scroll titled %s"
   :identified-format "Scroll of %s"
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
  "Complete catalog of all items."
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

(def ^:private category->flavor-spec
  {:potion {:flavor-dimensions [{:key :adjective :render :pool}
                                {:key :color     :render :pool}]
            :flavor-pools {:adjective potion-adjectives
                           :color     potion-colors}}

   :scroll {:flavor-dimensions [{:key :title :render :syllables}]}})

(defn- render-syllables
  [scroll]
  "")

(defn- dimension-word
  [dimension pool flavor]
  (let [dim-key (:key dimension)
        render (:render dimension)]
    (case render
      :pool (let [word (get-in pool [dim-key (dim-key flavor) :word])]
              (when (nil? word)
                (throw (ex-info "pool does not contain a word for this flavor"
                                {:dim-key dim-key :flavor flavor})))
              word)

      :syllables (render-syllables (dim-key flavor)))))

(defn- format-unidentified
  [appearance words]
  (format (:unidentified-format appearance) (str/join " " words)))

(defn- format-identified
  [appearance name]
  (if-let [id-format (:identified-format appearance)]
    (format id-format name)
    name))

(defn- build-unidentified-name
  [appearance flavor]
  (let [category   (:category appearance)
        flavor-spec (category category->flavor-spec)
        dimensions (:flavor-dimensions flavor-spec)
        pool       (:flavor-pools flavor-spec)
        desc (map #(dimension-word % pool flavor) dimensions)]
    (format-unidentified appearance desc)))

(defn display-name
  "Builds the display name for the item. For weapons, displays the :name field,
  but there is logic for potions and scrolls. Takes the flavors combos that the
  world rolled along with what the player knows. If the player already knows the
  item or there is no flavor for the item, just display the true name. Otherwise
  we show the 'unidentified' name."
  [item flavors known]
  (let [item-kind (:item/kind item)
        appearance (item-kind kind->appearance)]
    (when (nil? appearance)
      (throw (ex-info "given item kind has no appearance"
                      {:item/kind item-kind})))
    (let [flavor (item-kind flavors)]
      (if (or (known item-kind)
              (nil? flavor))
        (format-identified appearance (:name appearance))
        (build-unidentified-name appearance flavor)))))

(defn color-of
  [item flavors]
  (let [item-kind (:item/kind item)
        appearance (item-kind kind->appearance)
        flavor (flavors item-kind)
        category (:category appearance)]
    (or (when-let [c (:color flavor)]
          (get-in category->flavor-spec [category :flavor-pools :color c :color]))
        (:color appearance))))

(defn glyph-of
  [item-kind]
  (:glyph (item-kind kind->appearance)))
