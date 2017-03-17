(ns vim-simulator.core-test
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]))

(def state
  {:buffer [""]
   :cursor {:x 0 :y 0}})

(def event-insert
  {:vim-simulator/event "iHELLO^"})

(defn
  to-command
  ([event]
   (letfn [(extract-payload
            [description]
            (apply str (butlast (rest description))))]
    {:vim-simulator/command :vim-simulator/insert
     :vim-simulator/payload (extract-payload (:vim-simulator/event event))})))

(defn
  apply-to
  [state command]
  (case (:vim-simulator/command command)
    :vim-simulator/insert
    (let [y (get-in state [:cursor :y])
          line (get-in state [:buffer y])
          new-line (str line (:vim-simulator/payload command))]
      (assoc-in state [:buffer y] new-line))))


(defn
  process
  [state event]
  (->>
    event
    to-command
    (apply-to state)))
