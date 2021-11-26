(ns user
  (:require [caesarhu.shun-tools.or-tools.sat :as sat])
  (:import com.google.ortools.Loader
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback]))

(Loader/loadNativeLibraries)

(defn count-solutions
  []
  (let [counter (atom 0)
        model (CpModel.)
        numVals 3
        x (.newIntVar model 0 (dec numVals) "x")
        y (.newIntVar model 0 (dec numVals) "y")
        z (.newIntVar model 0 (dec numVals) "z")
        _ (.addDifferent model x y)]
    (sat/solutions model [x y z])))