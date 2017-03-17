(ns vim-simulator.core-test
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]))

(def state
  {:buffer ["0123456" "1234567"]
   :cursor {:x 2 :y 0}})

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
    (let [x (get-in state [:cursor :x])
          y (get-in state [:cursor :y])
          line (get-in state [:buffer y])
          chunks (map #(apply str %) (split-at x line))
          new-line (str (first chunks) (:vim-simulator/payload command) (second chunks))]
      (assoc-in state [:buffer y] new-line))))


(defn
  process
  [state event]
  (->>
    event
    to-command
    (apply-to state)))
