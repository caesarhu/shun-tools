(ns caesarhu.shun-tools.or-tools.MyCpSolverSolutionCallback
  (:gen-class
   :init init
   :state state
   :extends CpSolverSolutionCallback
   :methods [[-getValues [] clojure.lang.PersistentVector]]
   :constructors {[^IntVar variables] []})
  (:import com.google.ortools.Loader
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback]))

(defn -init
  [^IntVar variables]
  (atom {:vars variables
         :values []}))

(defn -getValues
  [this]
  (:values @(.state this)))

(defn -onSolutionCallback
  [this]
  (let [state @(.state this)
        vars (:vars state)
        values (:values state)]
    (swap! state assoc :values (conj values (map #(.value %) vars)))))

