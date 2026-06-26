(ns roguelike.core
  (:require [lanterna.screen :as s]
            [roguelike.input :as input]
            [roguelike.world :as world]
            [roguelike.render :as render]
            [roguelike.update :as u]))

(defn run-game
  [screen]
  (loop [world (world/new-world)]
    (render/draw-world screen world)
    (let [key (s/get-key-blocking screen)
          action (input/key->action key)
          new-world (u/update-world world action)]
      (when (not= (:mode new-world) :quit)
        (recur new-world)))))

(defn -main [& args]
  (let [screen (s/get-screen :text)]
    (s/start screen)
    (run-game screen)
    (s/stop screen)))
