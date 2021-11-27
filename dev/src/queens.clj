(ns queens
  (:require [caesarhu.shun-tools.or-tools.sat :as sat]
            [clojure.math.combinatorics :as combo])
  (:import [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback LinearExpr]))

(defn linear-distinct
  [model linear-expr-seq]
  (let [add-different (fn [left right]
                        (.addDifferent model left right))]
    (doseq [pair (combo/combinations linear-expr-seq 2)]
      (apply add-different pair))))

(defn solve-queens
  [board-size]
  (let [model (sat/new-model)
        queens (mapv #(.newIntVar model 0 (dec board-size) (str "x" %)) (range board-size))
        diag1 (for [i (range board-size)]
                (LinearExpr/affine (queens i) 1 i))
        diag2 (for [i (range board-size)]
                (LinearExpr/affine (queens i) 1 (- i)))]
    (.addAllDifferent model (into-array queens))
    (linear-distinct model diag1)
    (linear-distinct model diag2)
    (sat/count-solutions model)))

(defn queens-clojure
  [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (combo/permutations (range 1 (inc n)))))

(comment
  (time (solve-queens 10))
  (time (count (queens-clojure 10)))
  )