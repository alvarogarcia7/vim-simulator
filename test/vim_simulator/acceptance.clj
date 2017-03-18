(ns vim-simulator.acceptance
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]
            [vim-simulator.common :refer :all]

            [midje.sweet :refer :all]))

(defn simulate
  [description state events expected]
  (let [final-state (process-multiple state (map event events))]
    (facts
      description
      (fact :acceptance
            "adds to the buffer"
            (:buffer final-state) => (:buffer expected))
      (fact :acceptance
            "modifies the cursor"
            (:cursor final-state) => (:cursor expected)))
    final-state))

(facts
  "acceptance tests about processing events"
  (facts
    "about append"
    (simulate
      "append on an empty buffer"
      (state-gen ["" ""]
                 {:x 0 :y 0})
      ["AHELLO^"]
      (state-gen ["HELLO" ""]
                 {:x 5 :y 0}))
    (simulate
      "append on a full buffer"
      (state-gen ["1234" "aaaa"]
                 {:x 0 :y 0})
      ["AHELLO^"]
      (state-gen ["1234HELLO" "aaaa"]
                 {:x 9 :y 0}))
    ))

(facts
  "acceptance tests about events that affect events"
  (facts
    "about redo"
    (simulate
      "redo an insertion"
      (state-gen ["" ""]
                 {:x 0 :y 0})
      ["AHELLO^" "r"]
      (state-gen ["HELLOHELLO" ""]
                 {:x 10 :y 0})))
  (facts
    "about undo"
    (simulate
      "append on an empty buffer"
      (state-gen ["" ""]
                 {:x 0 :y 0})
      ["AHELLO^" "u"]
      (state-gen ["" ""]
                 {:x 0 :y 0}))))
