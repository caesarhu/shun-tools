(ns user
  (:require [caesarhu.shun-tools.or-tools.zebra :as z])
  (:import com.google.ortools.Loader
           caesarhu.shun-tools.or-tools.MyCpSolverSolutionCallback
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback]))

(Loader/loadNativeLibraries)

(defn count-solutions
  []
  (let [model (CpModel.)
        numVals 3
        x (.newIntVar model 0 (dec numVals) "x")
        y (.newIntVar model 0 (dec numVals) "y")
        z (.newIntVar model 0 (dec numVals) "z")
        _ (.addDifferent model x y)
        cb (new MyCpSolverSolutionCallback (to-array [x y z]))
        solver (CpSolver.)]
    (.setEnumerateAllSolutions (.getParameters solver) true)
    (.solve solver model cb)))