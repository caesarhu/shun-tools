(ns caesarhu.shun-tools.floyd-cycle)

; Floyd Cycle Detection Algorithm, 又稱為龜免賽跑算法

(defn double-eval
  [f state]
  (when-let [v (f state)]
    (f v)))

(defn cycle-detect
  "cycle detect, 判斷循環是否存在?
   detected: return match state
   not detected: nil"
  [f init-state]
  (loop [tortoise init-state
         hare init-state]
    (let [next-tortoise (f tortoise)
          next-hare (double-eval f hare)]
      (if (or (nil? next-hare) (= next-tortoise next-hare))
        next-hare
        (recur next-tortoise next-hare)))))

(defn cycle-start
  "find cycle start state, 定位循環的起始狀態
   return cycle start state"
  [f init-state catch-state]
  (->> (iterate #(map f %) [init-state catch-state])
       (some #(and (apply = %) %))
       first))

(defn cycle-states
  "Return a sequence of cycle states, first state is starter.
   取得所有循環狀態，第一個就是循環起點"
  [f start-state]
  (->> (iterate f (f start-state))
       (take-while #(not= start-state %))
       (cons start-state)))

(defn floyd-cycle
  "If existed cycle, return cycle states, else return nil
   如循環存在，返回循環的所有狀態，否則返回nil"
  [f init-state]
  (when-let [catch-state (cycle-detect f init-state)]
    (->> (cycle-start f init-state catch-state)
         (cycle-states f))))