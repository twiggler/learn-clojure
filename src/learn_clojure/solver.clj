(ns learn-clojure.solver)

(defrecord Puzzle [initial-state solved? moves move])

(defn breadth-first-search [{:keys [initial-state solved? moves move]} max-iter]
  (loop [[candidate & others] (map vector (moves initial-state)) visited #{} iter 0]
    (let [current (reduce move initial-state candidate)]
      (cond
        (solved? current) candidate
        (visited current) (recur others (conj visited current) (inc iter))
        (> iter max-iter) nil
        :else (recur
                (concat others (map #(conj candidate %) (moves current)))
                (conj visited current)
                (inc iter))))))
