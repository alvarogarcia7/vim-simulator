(ns vim-simulator.common)

(defn
  state-gen
  [buffer cursor]
  {:buffer buffer
   :cursor cursor})

(defn command [description]
  {:vim-simulator/command description})
