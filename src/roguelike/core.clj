(ns roguelike.core
  (:require [roguelike.input :as input]
            [roguelike.world :as world]
            [roguelike.render :as render]
            [roguelike.update :as u])
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
  (loop [world (world/new-world)]
    (render/draw-world screen world)
    (let [key-event (input/read-key screen)
          action (input/key->action key-event (:mode (:ui world)))
          new-world (u/update-world world action)]
      (when (not= (:mode (:ui new-world)) :quit)
        (recur new-world)))))

(defn -main [& args]
  (let [screen (get-new-screen)]
    (.startScreen screen)
    (run-game screen)
    (.stopScreen screen)))
