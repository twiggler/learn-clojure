(ns learn-clojure.board
  (:require [clojure.set :as set]
            [learn-clojure.solver :as solver]))

(defrecord Move [piece from to]
  solver/Move
  (negate [this] (->Move (:piece this) (:to this) (:from this))))

(defn transitions [edges]
  (let [
        m1 (map #(hash-map (first %) [(second %)]) edges)
        m2 (map #(hash-map (second %) [(first %)]) edges)]
    (apply (partial merge-with into) (concat m1 m2))))

(defn free-positions [transitions state piece]
  (into [] (set/difference (set (transitions (get state piece))) (set (vals state)))))

(defn moves [transitions state]
  (->>
    (keys state)
    (map (fn [piece] [[piece] (free-positions transitions state piece)]))
    (mapcat #(for [piece (first %) position (second %)] (->Move piece (get state piece) position)))))
