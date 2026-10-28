(ns roguelike.ui.item-select
  "The item picker shared by inventory, quaff, read, etc. Each screen is the
   same picker with a different mode frame:

  {:screen    :item-select
   :prompt    \"Quaff which potion?\"
   :filter    {:item/verb :quaff}     ; item must have every key/value listed
   :empty     \"You have no potions to quaff.\"
   :on-select {:action/type :world/quaff}}  ; sent with :item-id attached

  Add :quantity-prompt (e.g. \"Drop how many?\") to ask how many of a stack to
  use; the pick then goes through the number picker and :quantity is attached."
  (:require [roguelike.ui.appearance.item :as item]))

(defn- matches?
  [criteria item]
  (every? (fn [[k v]] (= v (get item k))) criteria))

(defn selectable
  "The pack entries the picker offers. Letters come from the full pack, so an
  item keeps its letter on every screen."
  [pack criteria]
  (filterv #(matches? criteria (:item %)) pack))

(defn find-entry
  [entries label]
  (first (filter #(= label (:label %)) entries)))

(defn rows
  "Turns selectable entries into display rows:
  [{:label \"a) \" :text \"2 bubbling azure potions\" :color :blue} ...]"
  [entries flavors known]
  (mapv (fn [{:keys [label item]}]
          {:label (str label ") ")
           :text  (item/describe item flavors known)
           :color (item/color-of item flavors)})
        entries))

(defn header
  "The top line, e.g. \"Select Item: (Inven: a-c, ESC)\"."
  [prompt entries]
  (str prompt " (Inventory: " (:label (first entries)) "-" (:label (last entries)) ", ESC)"))
