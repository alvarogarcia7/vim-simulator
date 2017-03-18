(ns vim-simulator.core
  (:gen-class))

(defmulti
  command
  (fn [description] (first (:vim-simulator/event description))))

(defmethod command \u [event]
  {:vim-simulator/command :vim-simulator/undo
   :vim-simulator/payload (rest (:vim-simulator/event event))})

(defmethod command \r [event]
  {:vim-simulator/command :vim-simulator/redo
   :vim-simulator/payload (rest (:vim-simulator/event event))})

(defn extract-payload
  [description]
  (apply str (butlast (rest description))))

(defmethod command \i [event]
    {:vim-simulator/command :vim-simulator/insert
     :vim-simulator/payload (extract-payload (:vim-simulator/event event))})

(defmethod command \A [event]
    {:vim-simulator/command :vim-simulator/append-at-end
     :vim-simulator/payload (extract-payload (:vim-simulator/event event))})

(defn
  pairs
  [coll]
  (first (reduce (fn [[acc prev] ele] [(conj acc [prev ele]) ele]) [[] (first coll)] (rest coll))))

(defn-
  flat1
  [coll]
  (reduce concat coll))

(defn
  duplicate-if-redo
  [events]
  (letfn [(redo? [event]
            (= (:vim-simulator/command event) :vim-simulator/redo))
          (remove-or-duplicate-redo
            [pairs]
            (map
              (fn [[p1 p2]] (if (redo? p2) [p1 p1] (if (redo? p1) [p2] [p1 p2]))) pairs))]
    (let [pairs (pairs events)]
      (into [] (flat1 (remove-or-duplicate-redo pairs))))))

(defn-
  apply-event-altering-events
  [events]
  (letfn [(undo? [event]
            (= (:vim-simulator/command event) :vim-simulator/undo))
          (discard-if-undo [acc ele]
            (if (undo? ele)
              (butlast acc)
              (conj acc ele)))]
    (let [e1 (reduce discard-if-undo [] events)]
      (if (> (count e1) 1) (duplicate-if-redo e1) e1))))

(defn-
  process-single
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
  process-multiple
  [state events]
  (->> events
       (map command)
       apply-event-altering-events
       (reduce process-single state)))
