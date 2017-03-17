(ns vim-simulator.core-test
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]
            [midje.sweet :refer :all]))

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
     {:vim-simulator/command (case (first (:vim-simulator/event event))
                               \i :vim-simulator/insert
                               \A :vim-simulator/append-at-end)
      :vim-simulator/payload (extract-payload (:vim-simulator/event event))})))

(defn
  apply-to
  [state command]
  (let [x (get-in state [:cursor :x])
        y (get-in state [:cursor :y])
        line (get-in state [:buffer y])
        replace-in-buffer (fn [line-number new-line state] (assoc-in state [:buffer line-number] new-line))
        replace-cursor (fn [x state] (assoc-in state [:cursor :x] x))]
    (case (:vim-simulator/command command)
      :vim-simulator/insert
      (let [[pre post] (map #(apply str %) (split-at (dec x) line))
            new-line (str pre (:vim-simulator/payload command) post)]
        (replace-in-buffer y new-line state))

      :vim-simulator/append-at-end
      (let [new-line (str line (:vim-simulator/payload command))]
        (replace-cursor (count new-line) (replace-in-buffer y new-line state))))))


(defn
  process
  [state event]
  (->>
    event
    to-command
    (apply-to state)))

(facts
  "about processing events"
  (facts
    "about insert"
    (fact
      "adds to the buffer"
      (let [next-state (process
                         {:buffer ["" ""]
                          :cursor {:x 0 :y 0}}
                         {:vim-simulator/event "iHELLO^"})]
        (:buffer next-state) => ["HELLO" ""]))))

;; how to use
;; (reduce (fn [acc ele] (process acc ele)) state [event-append-end-of-line event-insert])
;; equivalent
;; (reduce process state [event-append-end-of-line event-insert])
