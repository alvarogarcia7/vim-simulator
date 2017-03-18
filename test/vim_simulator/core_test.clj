(ns vim-simulator.core-test
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]
            [midje.sweet :refer :all]))

(def state
  {:buffer ["0123456" "1234567"]
   :cursor {:x 2 :y 0}})

(def event-insert
  {:vim-simulator/event "iHELLO^"})

(def event-undo
  {:vim-simulator/event "u"})

(def event-append
  {:vim-simulator/event "A at the end^"})

(defn event [description]
  {:vim-simulator/event description})

(defn
  process-multiple
  [state events]
  (reduce process state events))


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
    (process-multiple (state-gen [""] {:x 0 :y 0}) [(event "AHELLO^") (event "A BYE!^")] ) => {:buffer ["HELLO BYE!"]
                                                                               :cursor {:x 10 :y 0}}

    ))

;; how to use
;; (reduce (fn [acc ele] (process acc ele)) state [event-append-end-of-line event-insert])
;; equivalent
;; (reduce process state [event-append-end-of-line event-insert])
