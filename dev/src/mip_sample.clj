(ns mip-sample
  (:import [com.google.ortools Loader]
           [com.google.ortools.linearsolver MPSolver MPVariable MPObjective MPConstraint]))

(Loader/loadNativeLibraries)

(def infinity (MPSolver/infinity))

(defn mp-solver
  "type of solver, like GLOP"
  [type]
  (MPSolver/createSolver type))

(defn solve
  []
  (let [solver (mp-solver "SCIP")
        x (.makeIntVar solver 0.0 infinity "x")
        y (.makeIntVar solver 0.0 infinity "y")
        c0 (.makeConstraint solver (- infinity) 17.5 "c0")
        c1 (.makeConstraint solver (- infinity) 3.5 "c1")
        objective (.objective solver)]
    (.setCoefficient c0 x 1)
    (.setCoefficient c0 y 7)
    (.setCoefficient c1 x 1)
    (.setCoefficient c1 y 0)
    (.setCoefficient objective x 1)
    (.setCoefficient objective y 10)
    (.setMaximization objective)
    (let [status (.solve solver)]
      (if (= status com.google.ortools.linearsolver.MPSolver$ResultStatus/OPTIMAL)
        (do
          (println "Solution:")
          (println (str "Objective value = " (.value objective)))
          (println (str "x = " (.solutionValue x)))
          (println (str "y = " (.solutionValue y))))
        (println "The problem does not have an optimal solution!")))))