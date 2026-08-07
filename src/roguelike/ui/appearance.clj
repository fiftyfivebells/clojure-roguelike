(ns roguelike.ui.appearance
  (:require [roguelike.ui.appearance.monster :as monster]))

(def ^:private state->style
  {:visible    :bright
   :remembered :dim
   :unknown    :dark})

(def ^:private tile->glyph
  {:wall        "#"
   :floor       "."
   :closed-door "+"
   :open-door   "'"
   :stairs-down ">"
   :stairs-up   "<"
   :unknown     " "})

(defn tile-glyph
  [tile-kind]
  (tile->glyph tile-kind " "))

(defn state-style
  [state]
  (state->style state :dark))

(defn entity->glyph
  [entity]
  (case (:entity/type entity)
    :player "@"
    :monster (monster/glyph-of (:monster/genus entity))))

(defn entity->color
  [entity]
  (case (:entity/type entity)
    :player :pink
    :monster (monster/color-of (:monster/species entity))))
