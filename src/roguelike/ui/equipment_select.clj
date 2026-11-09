(ns roguelike.ui.equipment-select
  "The equipment list shared by the equipment screen and take off. Each screen
  is the same list with a different mode frame:

  {:screen    :equipment-select
   :prompt    \"Take off which item?\"
   :empty     \"You are not wearing or wielding anything.\"
   :on-select {:action/type :world/take-off}}  ; sent with :slot attached

  Like Angband, every slot has a fixed letter and empty slots are listed too."
  (:require [roguelike.ui.appearance.item :as item]))

(def ^:private slot-order [:main-hand :off-hand :body])

(def slot-names
  {:main-hand "main hand"
   :off-hand  "off hand"
   :body      "body"})

(defn entries
  "One entry per slot, in slot order: [{:label \\a :slot :main-hand :item {...}} ...].
  :item is nil for an empty slot."
  [equipment]
  (map-indexed (fn [i slot] {:label (char (+ (int \a) i)) :slot slot :item (get equipment slot)})
               slot-order))

(defn find-entry
  [entries label]
  (first (filter #(= label (:label %)) entries)))

(defn rows
  "Turns entries into display rows:
  [{:label \"a) \" :text \"main hand: a dagger\" :color :white} ...]"
  [entries flavors known]
  (mapv (fn [{:keys [label slot item]}]
          {:label (str label ") ")
           :text  (str (slot-names slot) ": "
                       (if item (item/describe item flavors known) "(nothing)"))
           :color (if item (item/color-of item flavors) :dim)})
        entries))

(defn header
  [prompt entries]
  (str prompt " (Equip: " (:label (first entries)) "-" (:label (last entries)) ", ESC)"))
