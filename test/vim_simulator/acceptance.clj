(ns vim-simulator.acceptance
  (:require [clojure.test :refer :all]
            [vim-simulator.core :refer :all]
            [midje.sweet :refer :all]))

(defn simulate
  [description state event expected]
  (let [next-state (process state {:vim-simulator/event event})]
    (facts
      description
      (fact :acceptance
            "adds to the buffer"
            (:buffer next-state) => (:buffer expected))
      (fact :acceptance
            "modifies the cursor"
            (:cursor next-state) => (:cursor expected)))
    next-state))

(facts
  "acceptance tests about processing events"
  (facts
    "about append"
    (simulate
      "append on an empty buffer"
      (state-gen ["" ""]
                 {:x 0 :y 0})
      "AHELLO^"
      (state-gen ["HELLO" ""]
                 {:x 5 :y 0}))
    (simulate
      "append on a full buffer"
      (state-gen ["1234" "aaaa"]
                 {:x 0 :y 0})
      "AHELLO^"
      (state-gen ["1234HELLO" "aaaa"]
                 {:x 9 :y 0}))
    ))

(facts
  "acceptance tests about events that affect events"
  (facts
    "about undo"
    (simulate "undo"
              (simulate
                "append on an empty buffer"
                (state-gen ["" ""]
                           {:x 0 :y 0})
                "AHELLO^"
                (state-gen ["HELLO" ""]
                           {:x 5 :y 0}))
              "u"
              (state-gen ["" ""]
                         {:x 0 :y 0}))
    ))

;; how to use
;; (reduce (fn [acc ele] (process acc ele)) state [event-append-end-of-line event-insert])
;; equivalent
;; (reduce process state [event-append-end-of-line event-insert])
