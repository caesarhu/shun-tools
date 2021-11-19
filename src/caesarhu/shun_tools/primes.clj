(ns caesarhu.shun-tools.primes
  (:require [clojure.math.numeric-tower :as math]))

(defn coprime? 
  "Two integers a and b are said to be coprime or relatively prime if
   the only positive integer that evenly divides both of them is 1."
  [a b]
  (= 1 (math/gcd a b)))

(defn sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (filter #(aget primes %)
                          (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (if (<= i n)
                  (do
                    (aset primes i false)
                    (recur (+ i p p))))))
            (recur  (+ p 2))))))))

(defn- test-prime
  "Determine if a number is prime by looping through divisors"
  [x]
  (loop [iter 5 top (math/sqrt x)]
    (cond
      (> iter top) true
      (or (zero? (mod x iter))
          (zero? (mod x (+ 2 iter)))) false
      :else (recur (+ 6 iter) top))))

(defn is-prime?
  [x]
  "Determines if a given integer is prime."
  (cond
    (<= x 3) (< 1 x)
    (or (zero? (mod x 2))
        (zero? (mod x 3))) false
    :else (test-prime x)))

(def primes
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve n]
            (if-let [step (sieve n)]
              (-> sieve
                  (dissoc n)
                  (enqueue n step))
              (enqueue sieve n (+ n n))))
          (next-primes [sieve n]
            (if (sieve n)
              (recur (next-sieve sieve n) (+ n 2))
              (cons n (lazy-seq (next-primes (next-sieve sieve n) (+ n 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

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
