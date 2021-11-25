(ns user
  (:require [caesarhu.shun-tools.or-tools.zebra :as z]
            [caesarhu.shun-tools.or-tools.MyCpSolverSolutionCallback])
  (:import com.google.ortools.Loader
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback]))

(Loader/loadNativeLibraries)

(defn count-solutions
  []
  (let [counter (atom 0)
        cb (proxy [CpSolverSolutionCallback] []
             (onSolutionCallback []
               (swap! counter inc)))
        model (CpModel.)
        numVals 3
        x (.newIntVar model 0 (dec numVals) "x")
        y (.newIntVar model 0 (dec numVals) "y")
        z (.newIntVar model 0 (dec numVals) "z")
        _ (.addDifferent model x y)
        solver (CpSolver.)]
    (.setEnumerateAllSolutions (.getParameters solver) true)
    (.solve solver model cb)
    @counter))