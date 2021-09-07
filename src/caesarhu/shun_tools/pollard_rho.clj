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

(defn double-eval
  [f x0]
  (f (f x0)))

(defn rho
  [x c n]
  (mod (+' (*' x x) c) n))

(defn frho
  [c n]
  #(rho % c n))

(defn floyd
  [c n]
  (let [next (frho c n)
        round 128
        round-set (set [1 2 4 8 16 32 64])]
    (loop [tortoise 0
           hare 0
           q 1
           step 1]
      (let [next-tortoise (next tortoise)
            next-hare (double-eval next hare)
            q (mod (*' q (math/abs (-' next-tortoise next-hare))) n)]
        (cond
          (or (zero? q) (= next-tortoise next-hare)) nil
          (or (round-set step) (zero? (mod step round)))
          (let [g (math/gcd q n)]
            (if (> g 1)
              g
              (recur next-tortoise next-hare q (inc step))))
          :else (recur next-tortoise next-hare q (inc step)))))))

(defn brent
  [c n]
  (let [next (frho c n)
        round 128]
    (loop [tortoise 0
           hare 0
           q 1
           step 1
           power 1]
      (let [next-hare (next hare)
            q (mod (*' q (math/abs (-' tortoise next-hare))) n)]
        (cond
          (or (zero? q) (= tortoise next-hare)) nil
          (or (= step power) (zero? (mod step round)))
          (let [g (math/gcd q n)]
            (if (> g 1)
              g
              (if (= step power)
                (recur next-hare next-hare q (inc step) (* 2 power))
                (recur tortoise next-hare q (inc step) power))))
          :else (recur tortoise next-hare q (inc step) power))))))

(defn pollard-rho
  ([n f]
   (cond
     (<= n 1) nil
     (= n 4) 2
     (deterministic-test n) n
     :else
     (loop [c (inc (rand-bigint (dec n)))]
       (if-let [d (f c n)]
         d
         (recur (inc (rand-bigint (dec n))))))))
  ([n]
   (pollard-rho n floyd)))

(defn- power-of
  [n p]
  (loop [n n
         power 0]
    (if (zero? (mod n p))
      (recur (quot n p) (inc power))
      power)))

(defn- factorization
  ([m f]
   (let [new-m (apply merge-with +
                      (for [[k v] m]
                        (let [x (f k)
                              power (power-of k x)
                              q (quot k (math/expt x power))
                              qm (when (> q 1)
                                   {q v})]
                          (merge-with + qm {x (* power v)}))))]
     (if (= m new-m)
       new-m
       (recur new-m f))))
  ([m]
   (factorization m pollard-rho)))

(defn prime-factors
  [n]
  (if (<= n 1)
    nil
    (factorization {n 1})))

(defn divisors
  [n]
  (->> (prime-factors n)
       (map (fn [[k v]] (repeat v k)))
       flatten
       (comb/subsets)
       (map #(apply * %))
       sort))

(comment
  (time (prime-factors 600851475143))
  (time (divisors 600851475143))
  ()
  )