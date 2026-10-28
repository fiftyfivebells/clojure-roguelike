(ns roguelike.ui.number-select
  "The number picker: drop how many, rest how long, etc. The player types into
  a text buffer kept in the mode frame, and Enter turns it into a number:

  {:screen    :number-select
   :prompt    \"Drop how many?\"
   :max       5
   :text      \"\"                      ; what the player has typed so far
   :on-select {:action/type :world/drop :item-id 51}}  ; sent with :quantity attached")

(def ^:private max-digits 4)

(defn type-char
  "Adds a typed character to the buffer. Digits append (up to max-digits); '*'
  means 'all' and replaces whatever was typed. Anything else is ignored."
  [text c]
  (cond
    (= c \*) "*"
    (not (Character/isDigit c)) text
    (= text "*") (str c)
    (>= (count text) max-digits) text
    :else (str text c)))

(defn erase
  "Removes the last typed character."
  [text]
  (subs text 0 (max 0 (dec (count text)))))

(defn value
  "The number the frame's buffer stands for, kept within 1..max. An empty
  buffer means 1 and '*' means max, like Angband's quantity prompt."
  [{:keys [text max]}]
  (case text
    ""  1
    "*" max
    (-> (parse-long text) (clojure.core/max 1) (min max))))

(defn prompt-line
  "The line shown while typing, e.g. \"Drop how many? (1-5, *=all): 3\"."
  [{:keys [prompt max text]}]
  (str prompt " (1-" max ", *=all): " text))
