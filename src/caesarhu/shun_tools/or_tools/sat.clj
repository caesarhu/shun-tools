(ns caesarhu.shun-tools.or-tools.sat
  (:import [com.google.ortools Loader]
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback]))

(Loader/loadNativeLibraries)

(defn solutions
  [model vars]
  (let [counter (atom 0)
        values (atom [])
        cb (proxy [CpSolverSolutionCallback] []
             (onSolutionCallback []
               (let [value (mapv #(.value this %) vars)]
                 (swap! values conj value))
               (swap! counter inc)))
        solver (CpSolver.)]
    (.setEnumerateAllSolutions (.getParameters solver) true)
    (.solve solver model cb)
    {:count @counter :values @values}))