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
   :append (to-command event-append)
   :redo (to-command (event "r"))})


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
            :cursor {:x 10 :y 0}})

  (fact :unit
        "undo the last two commands"
        (process-multiple
          initial-state
          [(event "AHELLO^") (event "A BYE!^") (event "u") (event "u")])
        => {:buffer [""], :cursor {:x 0, :y 0}}

        ))

(facts
  "generate pairs"
  (fact
    :unit
    "more than one element"
    (pairs [1 2 3]) => [[1 2] [2 3]]
    )
  (fact
    :unit
    "GOTCHA - not working for one element"
    (pairs [1]) => []
    ))
