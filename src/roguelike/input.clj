(ns roguelike.input
  (:require
    [roguelike.ui :as ui])
  (:import [com.googlecode.lanterna.input KeyType]))

(defn drain-input!
  "Repeatedly polls for input until the queue is empty. This is intended to stop
  'lagginess' caused by keypresses building up in the input queue while the
   world is advancing."
  [screen]
  (while (some? (.pollInput screen))))

(defn read-key
  "Reads a key from the terminal. Returns a map with information about the key press."
  [screen]
  (let [keystroke (.readInput screen)]
    {:key      (.getCharacter keystroke)
     :ctrl?    (.isCtrlDown keystroke)
     :alt?     (.isAltDown keystroke)
     :key-type (.getKeyType keystroke)}))

(defn play-mode
  "Play mode input handler. When the world is in :play mode, it reads movement
   keys and other commands and then returns a command type depending on the
   input."
  [key-event]
  (cond
    (and (:ctrl? key-event) (= (:key key-event) \x))
    {:action/type :ui/prompt
     :message "Really quit? (y to confirm, any other key to cancel)"
     :on-yes {:action/type :ui/quit}}

    ;; actions (get, quaff, etc.)
    (= (:key key-event) \i) {:action/type :ui/item-select
                             :prompt "Select Item:"
                             :filter {}
                             :empty "You have nothing in your inventory."}

    (= (:key key-event) \q) {:action/type :ui/item-select
                             :on-select {:action/type :world/quaff}
                             :prompt "Quaff which potion?"
                             :filter {:item/verb :quaff}
                             :empty "You have no potions to quaff."}

    (= (:key key-event) \g) {:action/type :world/get}

    ;; movement
    (= (:key key-event) \h) {:action/type :world/move :delta [-1 0]}
    (= (:key key-event) \y) {:action/type :world/move :delta [-1 -1]}
    (= (:key key-event) \b) {:action/type :world/move :delta [-1 1]}
    (= (:key key-event) \n) {:action/type :world/move :delta [1 1]}
    (= (:key key-event) \u) {:action/type :world/move :delta [1 -1]}
    (= (:key key-event) \l) {:action/type :world/move :delta [1 0]}
    (= (:key key-event) \j) {:action/type :world/move :delta [0 1]}
    (= (:key key-event) \k) {:action/type :world/move :delta [0 -1]}

    ;; travel/wait/etc
    (= (:key key-event) \.) {:action/type :world/wait}
    (= (:key key-event) \>) {:action/type :world/descend}
    (= (:key key-event) \<) {:action/type :world/ascend}

    :else {:action/type :ui/none}))

(defn prompt-mode
  "Prompt mode input handler. When the world is in :prompt mode, it reads the
   key input from the terminal. If the key pressed is a 'y', it sets the world to
   the mode provided. Any other key and it returns to the previous mode."
  [key-event mode]
  (if (= (:key key-event) \y)
    (:on-yes mode)
    {:action/type :ui/return}))

(defn item-select-mode
  "Item picker input handler. ESC backs out, and any character is sent as a
  pick; the game decides whether that letter means anything."
  [key-event]
  (condp = (:key-type key-event)
    KeyType/Escape    {:action/type :ui/return}
    KeyType/Character {:action/type :ui/select :label (:key key-event)}
    {:action/type :ui/none}))

(defn dead-mode
  []
  {:action/type :ui/quit})

(defn key->action
  "Dispatcher: depending on the screen that the world's mode is set to, it
   delegates to the appropriate  handler for the screen type."
  [pressed mode]
  (case (:screen mode)
    :dead (dead-mode)
    :prompt (prompt-mode pressed mode)
    :item-select (item-select-mode pressed)

    ;; default:
    ;; TODO: maybe refactor this so play isn't the default
    (play-mode pressed)))
