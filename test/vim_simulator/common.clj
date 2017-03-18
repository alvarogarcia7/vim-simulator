(ns vim-simulator.common)

(defn
  state-gen
  [buffer cursor]
  {:buffer buffer
   :cursor cursor})

(defn event [description]
  {:vim-simulator/event description})
