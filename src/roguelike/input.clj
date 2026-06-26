(ns roguelike.input)

(defn key->action
  [pressed]
  (case pressed
    \h {:type :move :dx -1 :dy 0}
    \l {:type :move :dx 1  :dy 0}
    \j {:type :move :dx 0  :dy 1}
    \k {:type :move :dx 0  :dy -1}
    \q {:type :quit}
    {:type :none}))
