(ns learn-clojure.train
  (:require
    [clojure.string :as str]
    [learn-clojure.solver :as solver]))

(defrecord Move [from to letter]
  solver/Move
  (negate [this] (->Move (:to this) (:from this) (:letter this))))


(defrecord State [tr tt tb])
(def initial-state (->State "aust" "f" ""))

(defn solved? [state]
  (some #(= (str/join %) "stauf") (vals state)))

(defn moves [{:keys [tr tt tb]}]
  (let [from-tr (if (and (not-empty tr) (or (empty? tt) (empty? tb)))
                  [(->Move :tr :tt (first tr)) (->Move :tr :tb (first tr))] [])
        tt-tr (if (and (not-empty tt) (or (empty? tb) (= 1 (count tt)))) (->Move :tt :tr (first tt)) nil)
        tt-tb (if (not-empty tt) (->Move :tt :tb (first tt)) nil)
        tb-tr (if (and (not-empty tb) (or empty? tt) (= 1 (count tb))) (->Move :tb :tr (first tb)) nil)
        tb-tt (if (not-empty tb) (->Move :tb :tt (first tb)) nil)
        ]
    (remove nil? (into from-tr [tt-tr tt-tb tb-tr tb-tt]))))

(defn move [state {:keys [from to letter]}]
  (-> state
      (update from rest)
      (update to #(conj (seq %) letter))))

(def train (solver/->Puzzle initial-state solved? (fn [_] false) (solver/make-state-scribe) moves move))


