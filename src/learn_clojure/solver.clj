(ns learn-clojure.solver)

(defrecord Puzzle [initial-state solved? lost? initial-scribe moves move])

(defrecord Context [state history])

(defrecord Candidate [move context])

(defprotocol Move
  (negate [this]))

(defprotocol Scribe
  (visited? [this context])
  (push [this context]))

(defrecord StateScribe [states]
  Scribe
  (visited? [this context] (contains? (:states this) (:state context)))
  (push [this context] (->StateScribe (conj (:states this) (:state context)))))

(defn make-state-scribe [] (->StateScribe #{}))

(defn filter-complement-move [last-move]
  (filter #(not= % (negate last-move))))

(defn wrap-in-context [context]
  (map #(->Candidate %1 context)))

(defn breadth-first-search [{:keys [initial-state solved? lost? initial-scribe moves move]} max-iter]
  (loop [[candidate & others] (sequence (wrap-in-context (->Context initial-state [])) (moves initial-state))
         scribe initial-scribe
         iter 0]
    (let [{{:keys [state history]} :context candidate-move :move} candidate
          next-context (->Context (move state candidate-move) (conj history candidate-move))
          filter-and-wrap (comp (filter-complement-move candidate-move) (wrap-in-context next-context))
          child-moves (sequence filter-and-wrap (moves (:state next-context)))]
      (cond
        (solved? (:state next-context)) (:history next-context)
        (> iter max-iter) nil
        (visited? scribe next-context) (recur others scribe (inc iter))
        (lost? next-context) (recur others (push scribe next-context) (inc iter))
        :else (recur
               (concat others child-moves)
               (push scribe next-context)
               (inc iter))))))
