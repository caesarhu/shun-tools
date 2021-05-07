(ns caesarhu.shun-tools.a-star
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn A*-search
  "Performs an a* search over the data using heuristic est-cost.
  ref from https://roboloco.net/project-euler/problem-81/

  ARGUMENTS:
    est-cost(e)  The estimate of the cost to the goal.
    neighbors(e) Returns map of neighbors at i,j and their costs
    start        The element at which to start
    goal?(e)     Function that checks if the goal has been reached.

  RETURNS:
    The list of elements in the optimal path. "
  [est-cost neighbors start goal?]
  (loop [open   (priority-map start [0 nil])
         closed {}]
    (let [[e [s p]] (first open)]
      (cond
        (nil? e)  "Path not found! No more elements to try!"
        (goal? e) (->> (conj (iterate #(second (closed %)) p) e)
                       (take-while #(not (nil? %)))
                       reverse)
        :else (recur
                (merge-with #(min-key first %1 %2) ;#(if (< (first %1) (first %2)) %1 %2)
                            (dissoc open e)
                            (into {} (for [[n ns] (neighbors e)
                                           :when (not (get closed n))]
                                       [n [(+ ns s (est-cost n)) e]])))
                (assoc closed e [s p]))))))