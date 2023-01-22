(ns learn-clojure.spiders
  (:require [learn-clojure.solver :as solver]
            [learn-clojure.board :as board]))

(def edges [[0 5] [0 3] [1 4] [1 6] [2 5] [2 7] [3 6] [4 7]])

(def routes (board/transitions edges))

; Two white and two black spiders
(defrecord SpiderPositions [w1 w2 b1 b2])

(def initial-state (->SpiderPositions 5 3 7 1))

(defn solved? [state]
  (and
    (or (= (:w1 state) 1) (= (:w1 state) 7))
    (or (= (:w2 state) 1) (= (:w2 state) 7))
    (or (= (:b1 state) 5) (= (:b1 state) 3))
    (or (= (:b2 state) 5) (= (:b2 state) 3))))

(defn lost? [context]
  (->>
    (:history context)
    (map :piece)
    (partition 2 1 [0])
    (reduce (fn [agg [first second]] (if (not= first second) (inc agg) agg)) 1)
    (< 7)))

(defn visited? [_] false)

(defn move [state {:keys [piece _ to] :as m}]
  map->SpiderPositions (assoc state piece to))

(def Spiders (solver/->Puzzle initial-state solved? lost? visited? (partial board/moves routes) move))