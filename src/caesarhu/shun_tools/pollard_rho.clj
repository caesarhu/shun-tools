(ns caesarhu.shun-tools.pollard-rho
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.miller-rabin :refer [deterministic-test]]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]))

(defn rand-bigint
  [n]
  (if (< n (misc/to-number (repeat 31 1) 2))
    (rand-int n)
    (let [length (count (misc/digits n 2))]
      (-> (repeatedly length #(rand-int 2))
          (misc/to-number 2)
          (mod n)))))

(defn- rho
  [x c n]
  (mod (+' (*' x x) c) n))

(defn- double-rho
  [x c n]
  (-> (rho x c n)
      (rho c n)))

(defn pollard-rho
  [n]
  (let [rand-n #(inc (rand-bigint (dec n)))
        round 256
        round-set (set [0 1 2 4 8 16 32 64 128])]
    (cond
      (= n 4) 2
      (deterministic-test n) n
      :else
      (loop [c (rand-n)
             t 0
             r 0
             step 0
             q 1]
        (let [t (rho t c n)
              r (double-rho r c n)
              q (mod (*' q (math/abs (-' t r))) n)]
          (cond
            (or (zero? q) (= t r)) (recur (rand-n) 0 0 (inc step) 1)
            (round-set (mod step round)) (let [d (math/gcd q n)]
                                           (if (> d 1)
                                             d
                                             (recur c t r (inc step) q)))
            :else (recur c t r (inc step) q)))))))

(defn- power-of
  [n p]
  (let [[q power] (last (for [power (range)
                              :let [pp (math/expt p power)]
                              :while (zero? (mod n pp))]
                          [(quot n pp) power]))]
    [q [p power]]))

(defn- --factorization
  [m]
  (let [new-m (apply merge-with +
                     (for [[k v] m]
                       (let [x (pollard-rho k)
                             [q [p power]] (power-of k x)
                             qm (when (> q 1)
                                  {q v})]
                         (merge-with + qm {p (* power v)}))))]
    (if (= m new-m)
      new-m
      (recur new-m))))

(defn prime-factors
  [n]
  (--factorization {n 1}))

(defn divisors
  [n]
  (->> (prime-factors n)
       (map (fn [[k v]] (repeat v k)))
       flatten
       (comb/subsets)
       (map #(apply * %))))

(comment
  (time (prime-factors (rand-bigint (misc/to-number (repeat 32 9)))))
  (time (prime-factors 57121))
  (time (divisors 1000))
  (time (prime-factors (*' 1238926361552897 93461639715357977769163558199606896584051237541638188580280321)))
  )