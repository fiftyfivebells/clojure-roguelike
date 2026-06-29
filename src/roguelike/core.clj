(ns roguelike.core
  (:require [roguelike.input :as input]
            [roguelike.world :as world]
            [roguelike.render :as render]
            [roguelike.update :as u])
  (:import [com.googlecode.lanterna.terminal DefaultTerminalFactory]))

(defn run-game
  [screen]
  (loop [world (world/new-world)]
    (render/draw-world screen world)
    (let [key-event (input/read-key screen)
          action (input/key->action key-event (:mode world))
          new-world (u/update-world world action)]
      (when (not= (:mode new-world) :quit)
        (recur new-world)))))

(defn -main [& args]
  (let [screen (.createScreen (DefaultTerminalFactory.))]
    (.startScreen screen)
    (run-game screen)
    (.stopScreen screen)))
