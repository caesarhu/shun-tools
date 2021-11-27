(ns caesarhu.shun-tools.or-tools.sat
  (:import [com.google.ortools Loader]
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback]))

(Loader/loadNativeLibraries)

(defn new-model
  []
  (CpModel.))

(defn new-solver
  []
  (CpSolver.))

(defn solve
  [model vars]
  (let [solver (new-solver)
        status (.solve solver model)]
    {:values (map #(.value solver %) vars) :status status}))

(defn solutions
  [model vars]
  (let [counter (atom 0)
        values (atom [])
        cb (proxy [CpSolverSolutionCallback] []
             (onSolutionCallback []
               (swap! values conj (mapv #(.value this %) vars))
               (swap! counter inc)))
        solver (new-solver)]
    (.setEnumerateAllSolutions (.getParameters solver) true)
    (let [status (.solve solver model cb)]
      {:count @counter :values @values :status status})))

(defn count-solutions
  [model vars]
  (let [counter (atom 0)
        cb (proxy [CpSolverSolutionCallback] []
             (onSolutionCallback []
               (swap! counter inc)))
        solver (new-solver)]
    (.setEnumerateAllSolutions (.getParameters solver) true)
    (let [status (.solve solver model cb)]
      {:count @counter :status status})))

