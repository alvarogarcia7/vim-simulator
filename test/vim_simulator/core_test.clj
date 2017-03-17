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
  [event]
  {:vim-simulator/command :vim-simulator/insert
   :vim-simulator/payload "HELLO"})

(defn
  apply-to
  [state command]
  (case (:vim-simulator/command command)
    :vim-simulator/insert
    (let [buffer (get state :buffer)
          line (nth buffer (:y (get state :cursor)))
          new-line (str line (:vim-simulator/payload command))]
      (assoc-in state [:buffer] new-line))))


(defn
  process
  [state event]
  (->>
    event
    to-command
    (apply-to state)))
