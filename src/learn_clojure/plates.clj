(ns learn-clojure.plates
  (:require
    [learn-clojure.solver :as solver]))

(def initial-state (into [] (range 0 10)))

(defn solved? [plate-positions]
  (= (frequencies plate-positions) {0 2, 2 2, 4 2, 6 2, 8 2}))

(defrecord Move [from to]
  solver/Move
  (negate [this] (->Move (:to this) (:from this))))

(defn plate-move [plate-positions]
  (let [single-start? (->> plate-positions
                           (take 3)
                           (apply distinct?))
        end-triple (->> plate-positions
                        (drop 3)
                        (take 3))
        single-end? (apply distinct? end-triple)
        start-pos (nth plate-positions 1)
        end-pos (nth end-triple 1)]
    (if (and single-start? single-end?) (->Move start-pos end-pos))))

(defn plate-moves [plate-positions]
  (->> plate-positions
       (cycle)
       (drop 9)
       (iterate (partial drop 1))
       (take 10)
       (keep plate-move)))

(defn moves [plate-positions]
  (->> plate-positions
       (sort)
       ((juxt plate-moves (comp plate-moves reverse)))
       (flatten)))


(defn move [plate-positions {:keys [from to]}]
  (replace {from to} plate-positions))

(def plates (solver/->Puzzle initial-state solved? (fn [_] false) (solver/make-state-scribe) moves move))
