(ns vim-simulator.core
  (:gen-class))

(defmulti
  event
  (fn [description] (first (:vim-simulator/command description))))

(defmethod event \u [command]
  {:vim-simulator/event   :vim-simulator/undo
   :vim-simulator/payload (rest (:vim-simulator/command command))})

(defmethod event \r [command]
  {:vim-simulator/event   :vim-simulator/redo
   :vim-simulator/payload (rest (:vim-simulator/command command))})

(defn extract-payload
  [description]
  (apply str (butlast (rest description))))

(defmethod event \i [command]
  {:vim-simulator/event   :vim-simulator/insert
   :vim-simulator/payload (extract-payload (:vim-simulator/command command))})

(defmethod event \A [command]
  {:vim-simulator/event   :vim-simulator/append-at-end
   :vim-simulator/payload (extract-payload (:vim-simulator/command command))})

(defn
  pairs
  [coll]
  ; from Functional programming patterns in scala and clojure, chapter 'tail recursion', page 145
  (partition 2 (interleave coll (rest coll))))

(defn-
  flat1
  [coll]
  (reduce concat coll))

(defn-
  duplicate-if-redo
  [events]
  (letfn [(redo? [event]
            (= (:vim-simulator/event event) :vim-simulator/redo))
          (remove-or-duplicate-redo
            [pairs]
            (map
              (fn [[p1 p2]] (if (redo? p2) [p1 p1] (if (redo? p1) [p2] [p1 p2]))) pairs))]
    (let [pairs (pairs events)]
      (into [] (flat1 (remove-or-duplicate-redo pairs))))))

(defn-
  apply-event-altering-commands
  [events]
  (letfn [(undo? [event]
            (= (:vim-simulator/event event) :vim-simulator/undo))
          (discard-if-undo [acc ele]
            (if (undo? ele)
              (butlast acc)
              (conj acc ele)))]
    (let [e1 (reduce discard-if-undo [] events)]
      (if (> (count e1) 1) (duplicate-if-redo e1) e1))))

(defn-
  process-single
  [state event]
  (let [x (get-in state [:cursor :x])
        y (get-in state [:cursor :y])
        line (get-in state [:buffer y])
        replace-in-buffer (fn [line-number new-line state] (assoc-in state [:buffer line-number] new-line))
        replace-cursor (fn [x state] (assoc-in state [:cursor :x] x))]
    (case (:vim-simulator/event event)
      :vim-simulator/insert
      (let [[pre post] (map #(apply str %) (split-at (dec x) line))
            new-line (str pre (:vim-simulator/payload event) post)]
        (replace-in-buffer y new-line state))

      :vim-simulator/append-at-end
      (let [new-line (str line (:vim-simulator/payload event))]
        (replace-cursor (count new-line) (replace-in-buffer y new-line state))))))

(defn
  process-multiple
  [state command]
  (->> command
       (map event)
       apply-event-altering-commands
       (reduce process-single state)))
