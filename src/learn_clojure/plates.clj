(ns learn-clojure.plates
  (:require
    [clojure.set :as set]
    [learn-clojure.solver :as solver]))

(def initial-state (into [] (range 0 10)))

(defn solved? [plate-positions]
  (= (frequencies plate-positions) {0 2, 2 2, 4 2, 6 2, 8 2}))

(defrecord Move [from to]
  solver/Move
  (negate [this] (->Move (:to this) (:from this))))

(defn plate-move [plate-positions start-position step]
  (loop [position start-position total-plates-passed 0]
    (let [norm-step (if (neg? step) (+ 10 step) step)
          next-position (mod (+ position norm-step) 10)
          current-plates (count (filter #{next-position} plate-positions))]
      (cond
        (and (= total-plates-passed 2) (= current-plates 1)) (->Move start-position next-position)
        (> total-plates-passed 2) nil
        :else (recur next-position (+ total-plates-passed current-plates))))))

(defn moves [plate-positions]
  (->> (frequencies plate-positions)
       (keep (fn [[k v]] (if (= v 1) k)))
       (mapcat #(vector (plate-move plate-positions % 1) (plate-move plate-positions % -1)))
       (remove nil?)))

(defn move [plate-positions {:keys [from to]}]
  (replace {from to} plate-positions))

(def plates (solver/->Puzzle initial-state solved? (fn [_] false) (solver/make-state-scribe) moves move))
