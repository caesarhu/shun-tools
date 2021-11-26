(ns user
  (:require [caesarhu.shun-tools.or-tools.zebra :as z]
            [caesarhu.shun-tools.or-tools.MyCpSolverSolutionCallback])
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
        _ (.addDifferent model x y)
        solver (CpSolver.)
        cb (proxy [CpSolverSolutionCallback] []
             (onSolutionCallback []
               (println (.value this x))
               (println (.value this y))
               (println (.value this z))
               (swap! counter inc)))]
    (.setEnumerateAllSolutions (.getParameters solver) true)
    (.solve solver model cb)
    @counter))