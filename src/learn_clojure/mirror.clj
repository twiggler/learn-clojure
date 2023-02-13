(ns learn-clojure.mirror
  (:require
    [learn-clojure.solver :as solver]))

(defrecord Move [from to]
  solver/Move
  (negate [this] (->Move (:to this) (:from this))))

(def goal (range 10))

(defn solved? [state]
  (let [zipped (map vector state goal)]
    (every? (fn [[s g]] (or (= s g) (nil? s))) zipped)))

(defn moves [state]
  (let [free-index (.indexOf state nil)
        left (if (> (mod free-index 5) 0) (- free-index 1) nil)
        right (if (< (mod free-index 5) 4) (+ free-index 1) nil)
        top (if (>= free-index 5) (- free-index 5) nil)
        bottom (if (<= free-index 4) (+ free-index 5) nil)
        ]
    (->> [left right top bottom]
         (remove nil?)
         (map #(->Move % free-index)))))

(defn move [state {:keys [from to]}]
  (assoc state to (state from) from nil))

(defn make-mirror [initial-state]
  (solver/->Puzzle initial-state solved? (fn [_] false) (solver/make-state-scribe) moves move))
