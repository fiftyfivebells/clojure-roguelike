(ns roguelike.number-select-test
  (:require [clojure.test :refer [deftest is]]
            [roguelike.game :as game]
            [roguelike.input :as input]
            [roguelike.ui :as ui]
            [roguelike.ui.number-select :as number-select])
  (:import [com.googlecode.lanterna.input KeyType]))

;; generating a game builds a whole dungeon (~200ms), and games are values,
;; so every test starts from this one
(def ^:private fresh-game (game/new-game))

(deftest typing-digits-appends
  (is (= "12" (reduce number-select/type-char "" [\1 \2]))))

(deftest typing-ignores-non-digits
  (is (= "1" (reduce number-select/type-char "" [\1 \x \-]))))

(deftest star-replaces-and-digits-replace-star
  (is (= "*" (number-select/type-char "12" \*)))
  (is (= "3" (number-select/type-char "*" \3))))

(deftest buffer-stops-at-four-digits
  (is (= "1234" (reduce number-select/type-char "" "123456"))))

(deftest erase-removes-last-char-and-is-safe-when-empty
  (is (= "1" (number-select/erase "12")))
  (is (= "" (number-select/erase ""))))

(deftest value-defaults-to-one-and-star-means-all
  (is (= 1 (number-select/value {:text "" :max 5})))
  (is (= 5 (number-select/value {:text "*" :max 5}))))

(deftest value-is-kept-within-one-to-max
  (is (= 3 (number-select/value {:text "3" :max 5})))
  (is (= 5 (number-select/value {:text "99" :max 5})))
  (is (= 1 (number-select/value {:text "0" :max 5}))))

(deftest prompt-line-shows-range-and-typed-text
  (is (= "Drop how many? (1-5, *=all): 3"
         (number-select/prompt-line {:prompt "Drop how many?" :max 5 :text "3"}))))

;; Through the ui: open, type, erase.

(def ^:private open
  {:action/type :ui/number-select
   :prompt      "Drop how many?"
   :max         5
   :on-select   {:action/type :world/drop :item-id 51}})

(defn- press
  [g key-event]
  (game/player-action g (input/key->action key-event (game/current-mode g))))

(deftest opening-pushes-an-empty-frame
  (is (= {:screen    :number-select
          :prompt    "Drop how many?"
          :max       5
          :text      ""
          :on-select {:action/type :world/drop :item-id 51}}
         (game/current-mode (game/player-action fresh-game open)))))

(deftest keys-edit-the-buffer
  (let [g (-> (game/player-action fresh-game open)
              (press {:key-type KeyType/Character :key \4})
              (press {:key-type KeyType/Character :key \2})
              (press {:key-type KeyType/Backspace}))]
    (is (= "4" (:text (game/current-mode g))))))

(deftest escape-closes-the-picker
  (let [g (-> (game/player-action fresh-game open)
              (press {:key-type KeyType/Escape}))]
    (is (nil? (game/current-mode g)))))

(deftest submit-sends-on-select-with-quantity
  (let [g (update fresh-game :ui ui/update-mode open)
        g (update g :ui ui/update-mode {:action/type :ui/type :char \3})
        [g action] (#'game/resolve-number g {:action/type :ui/submit})]
    (is (= {:action/type :world/drop :item-id 51 :quantity 3} action))
    (is (nil? (game/current-mode g)))))
