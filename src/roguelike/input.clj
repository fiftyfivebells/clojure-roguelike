(ns roguelike.input)

(defn read-key
  [screen]
  (let [keystroke (.readInput screen)]
    {:key      (.getCharacter keystroke)
     :ctrl?    (.isCtrlDown keystroke)
     :alt?     (.isAltDown keystroke)
     :key-type (.getKeyType keystroke)}))

(defn play-mode
  [key-event]
  (cond
    (and (:ctrl? key-event) (= (:key key-event) \x)) {:type :prompt
        :message "Really quit? (y to confirm, any other key to cancel)"
        :on-yes {:type :quit}
                                                      :return {:screen :play}}

    (= (:key key-event) \h) {:type :move :dx -1 :dy 0}
    (= (:key key-event) \l) {:type :move :dx 1  :dy 0}
    (= (:key key-event) \j) {:type :move :dx 0  :dy 1}
    (= (:key key-event) \k) {:type :move :dx 0  :dy -1}

    :else {:type :none}))

(defn prompt-mode
  [key-event mode]
  (if (= (:key key-event) \y)
    (:on-yes mode)
    {:type :return}))

(defn key->action
  [pressed mode]
  (case (:screen mode)
    :play (play-mode pressed)
    :prompt (prompt-mode pressed mode)))
