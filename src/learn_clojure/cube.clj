(ns learn-clojure.cube
  (:require [learn-clojure.solver :as solver]
            [clojure.set :as set]))
(defrecord State [position pip-directions])

(def start-pos [0 0 0])
(def initial-state (->State start-pos {}))

(defrecord Move [direction forced]
  solver/Move
  (negate [this] (->Move (map - (:direction this)) (:forced this))))

(def pips [:start 4 1 5
           ; Plane 0
           4 4 6 3
           3 2 1 5
           1 6 5 2

           ; Plane 1
           5 2 6 4
           2 1 4 5
           3 5 3 2
           1 2 6 :finish

           ; Plane 2
           1 6 5 4
           5 4 6 3
           3 2 5 1
           6 1 3 6])

(defn lookup-pips [[x y z]]
  (pips (+ x (* y 4) (* z 4 4))))

(defn solved? [{:keys [position]}]
  (= position [3 3 1]))

(def directions
  (set (for [x [-1 0 1] y [-1 0 1]
             :when (> (+ (* x x) (* y y)) 0)]
         [x y])))

(defn destination-tile [[x y z] [dx dy]]
  {:pre [(<= -1 dx 1) (<= -1 dy 1)]}
  (let [ux (+ x dx)
        uy (+ y dy)
        bx (<= 0 ux 3)
        by (<= 0 uy 3)]
    (cond
      (and bx by) [ux uy z]
      (= bx by false) nil
      (and (> ux 3) (= z 0)) [0 uy 2]                       ; transition from plane 0 to plane 2
      (and (< ux 0) (= z 2)) [4 uy 0]                       ; transition from plane 2 to plane 0
      (and (> uy 3) (= z 0)) [ux 0 1]                       ; transition from plane 0 to plane 1
      (and (< uy 0) (= z 1)) [ux 3 0]                       ; transition from plane 1 to plane 0
      (and (> uy 3) (= z 2)) [3 ux 1]                       ; transition from plane 2 to plane 1
      (and (> ux 3) (= z 1)) [uy 3 2])))                    ; transition from plane 1 to plane 2

(defn moves [{:keys [position pip-directions]}]
  (if (= position start-pos)
    [(->Move [1 0] false) (->Move [0 1] false)]
    (if-let [direction (pip-directions (lookup-pips position))]
      (let [next-position (destination-tile position direction)]
        (if (and (some? next-position) (not= next-position start-pos))
          [(->Move direction true)]
          []))
      (->>
        (set/difference directions (vals pip-directions))
        (map #(->Move % false))
        (filter #(destination-tile position (:direction %)))))))

(defn move [{:keys [position pip-directions]} {:keys [direction]}]
  (let [next-position (destination-tile position direction)
        pips (lookup-pips position)
        next-pip-directions (if (number? pips) (assoc pip-directions pips direction) pip-directions)]
    (->State next-position next-pip-directions)))

(def cube (solver/->Puzzle initial-state solved? (fn [_] false) (solver/make-state-scribe) moves move))