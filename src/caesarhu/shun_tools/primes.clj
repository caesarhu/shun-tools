(ns caesarhu.shun-tools.primes
  (:require [clojure.math.numeric-tower :as math]))

(defn coprime? 
  "Two integers a and b are said to be coprime or relatively prime if
   the only positive integer that evenly divides both of them is 1."
  [a b]
  (= 1 (math/gcd a b)))

(defn- test-prime
  "Determine if a number is prime by looping through divisors"
  [x]
  (loop [iter 5 top (math/sqrt x)]
    (cond
      (> iter top) true
      (or (zero? (mod x iter))
          (zero? (mod x (+ 2 iter)))) false
      :else (recur (+ 6 iter) top))))

(def is-prime?
  "Determines if a given integer is prime."
  (memoize
    (fn [x]
      (cond
        (<= x 3) (< 1 x)
        (or (zero? (mod x 2))
            (zero? (mod x 3))) false
        :else (test-prime x)))))

(def primes
  (concat 
   [2 3 5 7]
   (lazy-seq
    (let [primes-from (fn primes-from [n [f & r]]
                        (if (some #(zero? (rem n %))
                                  (take-while #(<= (* % %) n) primes))
                          (recur (+ n f) r)
                          (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn prime-factors-of
  [n]
  (let [sqr (first (math/exact-integer-sqrt n))]
    (loop [n n
           prime-seq primes
           result []]
      (let [p (first prime-seq)]
        (cond
          (or (> p sqr) (= n p)) (cons n result)
          (zero? (rem n p)) (recur (quot n p) prime-seq (cons p result))
          :else (recur n (next prime-seq) result))))))

(def composites 
  (remove is-prime? (iterate inc 2)))

(defn primes-after [n]
  (let [next-prime (.nextProbablePrime (BigInteger/valueOf n))]
    (cons next-prime (lazy-seq (primes-after next-prime)))))

(defn primes-range [x y]
  (->> primes
       (drop-while #(< % x))
       (take-while #(<= % y))))

(defn quadratfrei? 
  "A number is said to be squarefree (or sometimes quadratfrei; Shanks 1993)
   if its prime decomposition contains no repeated factors. All primes are 
   therefore trivially squarefree. The number 1 is by convention taken to be
   squarefree. The squarefree numbers are 1, 2, 3, 5, 6, 7, 10, 11, 13, 14,
   15, ... (Sloane's A005117). The squareful numbers (i.e., those that 
   contain at least one square) are 4, 8, 9, 12, 16, 18, 20, 24, 25, ... 
   (Sloane's A013929)."
  [n]
  (->> (prime-factors-of n)
       frequencies
       (every? #(= (second %) 1))))

(defn phi 
  "Euler's totient or phi function, φ(n) is an arithmetic function that
   counts the number of positive integers less than or equal to n that 
   are relatively prime to n. That is, if n is a positive integer, then 
   φ(n) is the number of integers k in the range 1 ≤ k ≤ n for which 
   gcd(n, k) = 1"
  [n]
  (->> (prime-factors-of n)
       distinct
       (map #(- 1 (/ 1 %))) 
       (reduce * n)))
