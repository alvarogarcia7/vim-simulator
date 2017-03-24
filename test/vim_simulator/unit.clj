(ns vim-simulator.unit
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]
            [vim-simulator.common :refer :all]
            [midje.sweet :refer :all]))

(def initial-state
  (state-gen [""] {:x 0 :y 0}))

(def commands
  {:undo   (command "u")
   :insert (command "iHELLO^")
   :append (command "A at the end^")
   :redo   (command "r")})

(facts
  "unit tests about parsing events"
  (fact :unit
        "about undo"
        (event (:undo commands))
        => {:vim-simulator/event   :vim-simulator/undo
            :vim-simulator/payload ()}
        ))

(facts
  "processing multiple events"
  (fact :unit
        "example 1"
        (process-multiple
          initial-state
          [(command "AHELLO^") (command "A BYE!^")])
        => {:buffer ["HELLO BYE!"]
            :cursor {:x 10 :y 0}})

  (fact :unit
        "undo the last two commands"
        (process-multiple
          initial-state
          [(command "AHELLO^") (command "A BYE!^") (command "u") (command "u")])
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
