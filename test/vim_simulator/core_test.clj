(ns vim-simulator.core-test
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]))

(def state
  {:buffer ["0123456" "1234567"]
   :cursor {:x 2 :y 0}})

(def event-insert
  {:vim-simulator/event "iHELLO^"})

(def event-append
  {:vim-simulator/event "A at the end^"})

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
          [pre post] (map #(apply str %) (split-at x line))
          new-line (str pre (:vim-simulator/payload command) post)]
      (assoc-in state [:buffer y] new-line))))


(defn
  process
  [state event]
  (->>
    event
    to-command
    (apply-to state)))
