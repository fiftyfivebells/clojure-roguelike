(ns roguelike.input
  (:require
    [roguelike.ui :as ui]
    [clojure.string :as str])
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

(defn- use-picker
  "Picker frame for commands that use one item. Takes in the verb, noun, and
  plural version so that we can return a consistent action. The output for
  [:quaff \"potion\" \"potions\"] will be:
  {:prompt \"Quaff which potion?\"
   :filter {:item/verb :quaff}
   :empty  \"You have no potions to quaff.\"
   :on-select {:action/type :world/quaff}
  }"
  [verb noun plural]
  (let [word (name verb)]
    {:prompt    (str (str/capitalize word) " which " noun "?")
     :filter    {:item/verb verb}
     :empty     (str "You have no " plural " to " word ".")
     :on-select {:action/type (keyword "world" word)}}))

(def ^:private item-pickers
  "Commands that open the item picker, by key. Each value is a picker frame
  (see roguelike.ui.item-select). :on-select is the action a pick sends, with
  :item-id attached; without one, picking does nothing."
  {\i {:prompt "Select Item:"
       :filter {}
       :empty  "You have nothing in your inventory."}
   \d {:prompt          "Select item to drop:"
       :filter          {}
       :empty           "You have nothing in your inventory."
       :quantity-prompt "Drop how many?"
       :on-select       {:action/type :world/drop}}
   \q (use-picker :quaff "potion" "potions")
   \r (use-picker :read  "scroll" "scrolls")
   \w {:prompt "Wear or wield which item?"
       :filter {:item/verb :wield}
       :empty "You have no items to wield/wear."
       :on-select {:action/type :world/wield}}})

(def ^:private equipment-pickers
  "Commands that open the equipment list, by key. Each value is a frame (see
  roguelike.ui.equipment-select). :on-select is the action a pick sends, with
  :slot attached; without one, picking does nothing."
  {\e {:prompt "Equipment:"
       :empty  "You are not wearing or wielding anything."}
   \t {:prompt    "Take off which item?"
       :empty     "You are not wearing or wielding anything."
       :on-select {:action/type :world/take-off}}})

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

    ;; actions (inventory, quaff, read, etc.)
    (contains? item-pickers (:key key-event))
    (assoc (item-pickers (:key key-event)) :action/type :ui/item-select)

    (contains? equipment-pickers (:key key-event))
    (assoc (equipment-pickers (:key key-event)) :action/type :ui/equipment-select)

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

(defn number-select-mode
  "Number picker input handler. ESC backs out, Enter submits, Backspace erases,
  and characters are typed into the buffer (the ui decides which ones count)."
  [key-event]
  (condp = (:key-type key-event)
    KeyType/Escape    {:action/type :ui/return}
    KeyType/Enter     {:action/type :ui/submit}
    KeyType/Backspace {:action/type :ui/erase}
    KeyType/Character {:action/type :ui/type :char (:key key-event)}
    {:action/type :ui/none}))

(defn choice-mode
  "Choice prompt input handler. A character is sent as the choice; the game
  decides whether it's one of the options. Any other key cancels."
  [key-event]
  (if (= KeyType/Character (:key-type key-event))
    {:action/type :ui/choose :label (:key key-event)}
    {:action/type :ui/return}))

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
    :equipment-select (item-select-mode pressed)
    :choice (choice-mode pressed)
    :number-select (number-select-mode pressed)
    ;; default:
    ;; TODO: maybe refactor this so play isn't the default
    (play-mode pressed)))
