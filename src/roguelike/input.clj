(ns roguelike.input)

(defn read-key
  "Reads a key from the terminal. Returns a map with information about the key press."
  [screen]
  (let [keystroke (.readInput screen)]
    {:key      (.getCharacter keystroke)
     :ctrl?    (.isCtrlDown keystroke)
     :alt?     (.isAltDown keystroke)
     :key-type (.getKeyType keystroke)}))

(defn play-mode
  "Play mode input handler. When the world is in :play mode, it reads movement keys and other commands
  and then returns a command type depending on the input."
  [key-event]
  (cond
    (and (:ctrl? key-event) (= (:key key-event) \x))
    {:type :ui/prompt
     :message "Really quit? (y to confirm, any other key to cancel)"
     :on-yes {:type :ui/quit}
     :return {:screen :play}}

    (= (:key key-event) \h) {:type :world/move :delta [-1 0]}
    (= (:key key-event) \y) {:type :world/move :delta [-1 -1]}
    (= (:key key-event) \b) {:type :world/move :delta [-1 1]}
    (= (:key key-event) \n) {:type :world/move :delta [1 1]}
    (= (:key key-event) \u) {:type :world/move :delta [1 -1]}
    (= (:key key-event) \l) {:type :world/move :delta [1 0]}
    (= (:key key-event) \j) {:type :world/move :delta [0 1]}
    (= (:key key-event) \k) {:type :world/move :delta [0 -1]}
    (= (:key key-event) \.) {:type :world/wait}

    :else {:type :ui/none}))

(defn prompt-mode
  "Prompt mode input handler. When the world is in :prompt mode, it reads the key input from the terminal.
  If the key pressed is a 'y', it sets the world to the mode provided. Any other key and it returns to the
  previous mode."
  [key-event mode]
  (if (= (:key key-event) \y)
    (:on-yes mode)
    {:type :ui/return}))

(defn key->action
  "Dispatcher: depending on the screen that the world's mode is set to, it delegates to the appropriate
  handler for the screen type."
  [pressed mode]
  (case (:screen mode)
    :play (play-mode pressed)
    :prompt (prompt-mode pressed mode)))
