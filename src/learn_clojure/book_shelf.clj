(ns learn-clojure.book-shelf
  (:require [learn-clojure.solver :as solver]))

(defrecord Shelf [green-indices empty-index])

(defrecord Move [index empty-index]
  solver/Move
  (negate [this] (->Move (:empty-index this) (:index this))))

; red green red green red green red green empty empty
(def initial-state
  (->Shelf #{1 3 5 7} 8))

; empty empty green green green green red red red red
(def goal
  (->Shelf #{2 3 4 5} 0))

(defn solved? [state] (= goal state))

(defn lost? [_] false)

(defn valid-state? [{:keys [green-indices empty-index]}]
  (and
    (set? green-indices)
    (= 4 (count green-indices))
    (every? #(<= 0 % 9) green-indices)
    (<= 0 empty-index 8)
    (not (contains? green-indices empty-index))
    (not (contains? green-indices (inc empty-index)))))

(defn move-book [green-indices source destination]
  (if (contains? green-indices source)
    (-> green-indices
        (disj source)
        (conj destination))
    green-indices))

(defn moves [{:keys [empty-index]}]
  (->> (range 9)
       (remove #{empty-index (inc empty-index)})
       (map #(->Move % empty-index))))

(defn move [{:keys [green-indices empty-index]} {:keys [index]}]
  {:pre  [(<= 0 index 8)]
   :post [(valid-state? %) (= (:empty-index %) index)]}
  (->Shelf
    (-> green-indices
        (move-book index empty-index)
        (move-book (inc index) (inc empty-index))
        )
    index))

(def BookShelf (solver/->Puzzle initial-state solved? lost? (solver/make-state-scribe) moves move))