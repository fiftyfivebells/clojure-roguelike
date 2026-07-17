(ns roguelike.core
  (:require [roguelike.input  :as input]
            [roguelike.render :as render]
            [roguelike.game   :as game])
  (:import [com.googlecode.lanterna.terminal DefaultTerminalFactory]
           [com.googlecode.lanterna.terminal.swing SwingTerminalFontConfiguration]
           [com.googlecode.lanterna TerminalSize]))

(defn- get-new-screen
  []
  (-> (DefaultTerminalFactory.)
      (.setTerminalEmulatorTitle "My Game")
      (.setInitialTerminalSize (TerminalSize. 80 25))
      (.setPreferTerminalEmulator true)   ; force Swing even when a TTY console is present
      (.setTerminalEmulatorFontConfiguration
       (SwingTerminalFontConfiguration/getDefaultOfSize 16))
      (.createScreen)))

(defn run-game
  [screen]
  (loop [[game status] (game/advance (game/new-game))]
    (render/draw-game screen game)
    (if (= status :awaiting-input)
      (let [key-event    (input/read-key screen)
            action       (input/key->action key-event (:mode (:ui game)))
            updated-game (game/player-action game action)]
        (when (not= (get-in updated-game [:ui :mode :screen]) :quit)
          (recur (game/advance updated-game))))
      (recur (game/advance game)))))

(defn -main [& args]
  (let [screen (get-new-screen)]
    (.startScreen screen)
    (run-game screen)
    (.stopScreen screen)))
