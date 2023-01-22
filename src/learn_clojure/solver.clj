(ns learn-clojure.solver)

(defrecord Puzzle [initial-state solved? lost? visited? moves move])

(defrecord Context [state history])

(defrecord Candidate [move context])
(defprotocol Move
  (negate [this]))

(defn filter-complement-move [last-move]
  (if last-move (filter #(not= % (negate last-move))) identity))

(defn wrap-in-context [context]
  (map #(->Candidate %1 context)))

(defn breadth-first-search [{:keys [initial-state solved? lost? visited? moves move]} max-iter]
  (loop [[candidate & others] (sequence (wrap-in-context (->Context initial-state [])) (moves initial-state))
         visited [(->Context initial-state [])]
         iter 0]
    (let [{{:keys [state history]} :context candidate-move :move} candidate
          next-context (->Context (move state candidate-move) (conj history candidate-move))
          filter-and-wrap (comp (filter-complement-move candidate-move) (wrap-in-context next-context))
          child-moves (sequence filter-and-wrap (moves (:state next-context)))]
      (cond
        (solved? (:state next-context)) (:history next-context)
        (> iter max-iter) nil
        (visited? next-context) (recur others visited (inc iter))
        (lost? next-context) (recur others (conj visited next-context) (inc iter))
        :else (recur
                (into child-moves others )
                (conj visited next-context)
                (inc iter))))))
