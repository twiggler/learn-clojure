(ns learn-clojure.chess
  (:require [learn-clojure.solver :as solver]
            [learn-clojure.board :as board]))

;; Board:
;; 0
;; 1 4 7 9
;; 2 5 8
;; 3 6

(def edges [[0 7] [0 5] [1 8] [1 6] [2 7] [3 4] [3 8] [5 9] [6 7]])

(def jumps (board/transitions edges))

; Two white and two black knights
(defrecord KnightPositions [w1 w2 b1 b2])

(def initial-state (->KnightPositions 5 9 0 2))

(defn solved? [state]
  (and
    (or (= (:w1 state) 0) (= (:w1 state) 2))
    (or (= (:w2 state) 0) (= (:w2 state) 2))
    (or (= (:b1 state) 5) (= (:b1 state) 9))
    (or (= (:b2 state) 5) (= (:b2 state) 9))))

(defn lost? [_] false)
(defn move [state {:keys [piece _ to]}]
  ->KnightPositions (assoc state piece to))

(def Knights (solver/->Puzzle initial-state solved? lost? (solver/make-state-scribe) (partial board/moves jumps) move))