(ns mp-sample
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
  (let [solver (mp-solver "GLOP")
        x (.makeNumVar solver 0.0 infinity "x")
        y (.makeNumVar solver 0.0 infinity "y")
        c0 (.makeConstraint solver (- infinity) 14.0 "c0")
        c1 (.makeConstraint solver 0.0 infinity "c1")
        c2 (.makeConstraint solver (- infinity) 2.0 "c2")
        objective (.objective solver)]
    (println (str "Number of variables = " (.numVariables solver)))
    (.setCoefficient c0 x 1)
    (.setCoefficient c0 y 2)
    (.setCoefficient c1 x 3)
    (.setCoefficient c1 y -1)
    (.setCoefficient c2 x 1)
    (.setCoefficient c2 y -1)
    (println (str "Number of constraints = " (.numConstraints solver)))
    (.setCoefficient objective x 3)
    (.setCoefficient objective y 4)
    (.setMaximization objective)
    (let [status (.solve solver)]
      (if (= status com.google.ortools.linearsolver.MPSolver$ResultStatus/OPTIMAL)
        (do
          (println "Solution:")
          (println (str "Objective value = " (.value objective)))
          (println (str "x = " (.solutionValue x)))
          (println (str "y = " (.solutionValue y))))
        (println "The problem does not have an optimal solution!")))))