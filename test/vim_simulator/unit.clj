(ns vim-simulator.unit
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]
            [midje.sweet :refer :all]))

;; States

(def state
  (state-gen ["0123456" "1234567"]
   {:x 2 :y 0}))

(def initial-state
  (state-gen [""] {:x 0 :y 0}))

(defn event [description]
  {:vim-simulator/event description})

;; Sample events

(def event-insert
  (event "iHELLO^"))

(def event-undo
  (event "u"))

(def event-append
  (event "A at the end^"))

(def commands
  {:undo   (to-command event-undo)
   :insert (to-command event-insert)
   :append (to-command event-append)})


;;undo
(defn
  undo?
  [event]
  (= (:vim-simulator/command event) :vim-simulator/undo))


(defn
  apply-undo
  [events]
  (reduce (fn [acc ele] (if (undo? ele)
                          (butlast acc)
                          (conj acc ele)))
          [] events))

(defn
  process-multiple
  [state events]
  (let [modified-events (apply-undo (map to-command events))]
    (reduce process-single state modified-events)))


(facts
  "unit tests about parsing events"
  (fact :unit
    "about undo"
    (to-command event-undo) => {:vim-simulator/command :vim-simulator/undo
                         :vim-simulator/payload ()}
    ))

(facts
  "processing multiple events"
  (fact :unit
        "example 1"
        (process-multiple
          initial-state
          [(event "AHELLO^") (event "A BYE!^")])
        => {:buffer ["HELLO BYE!"]
            :cursor {:x 10 :y 0}}))

;; how to use
;; (reduce (fn [acc ele] (process acc ele)) state [event-append-end-of-line event-insert])
;; equivalent
;; (reduce process state [event-append-end-of-line event-insert])
