(ns caesarhu.shun-tools.math-misc
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :refer :all]))

(def integers (iterate inc 1))

(def squares (->> integers (filter odd?) (reductions +)))

(defn is-square? [n]
  (zero? (last (math/exact-integer-sqrt n))))

(defn triangle [n] (quot (* n (inc n)) 2))

(defn square [n] (* n n))

(defn pentagon [n] (quot (* n (dec (* 3 n))) 2))

(defn hexagon [n] (* n (dec (* 2 n))))

(defn heptagon [n] (quot (* n (- (* 5 n) 3)) 2))

(defn octagon [n] (* n (- (* 3 n) 2)))

(defn number-seq [f] (map f integers))

(def triangle-numbers (reductions + integers))

(def hexagonal-numbers (take-nth 2 triangle-numbers))

(defn centred-hexagon [n] (inc (* 3 n (dec n))))

(def centred-hexagonal-numbers (map centred-hexagon integers))

(defn triangle-root [n]
  (/ (dec (math/sqrt (inc (* 8 n)))) 2))

(defn is-triangular? [n]
  (let [root (triangle-root n)]
    (== root (int root))))

(defn is-pentagonal? [x]
  (let [n (/ (inc (math/sqrt (inc (* 24 x)))) 6)]
    (== n (int n))))

(defn to-number 
  "Convert a collection of digits to a number"
  ([xs] (to-number xs 10))
  ([xs radix]
   (let [index (map #(math/expt radix %) (range))]
     (reduce + (map * index (reverse xs))))))

(defn char-to-int [c]
  (- (int c) 48))

;(defn digits [n]
;  (map char-to-int (str n)))

(defn digits 
  ([n] (digits n 10))
  ([n radix]
   (loop [n n
          res nil]
     (if (zero? n)
       res
       (recur
         (quot n radix)
         (cons (rem n radix) res))))))

(def factorial-seq
  (cons 1N ; the value of 0! is 1, according to the convention for an empty product.
    (->> (iterate inc 1)
         (map bigint)
         (reductions *))))

(defn factorial
  [n]
  (first (drop n factorial-seq)))

(defn sum-of [func n]
  (->> (digits n)
       (map func)
       (reduce +)))

(defn divisors
  [n]
  (let [f (fn [d xs]
              (cond
                (<= d 1)          (cons 1 xs)
                (zero? (rem n d)) (recur (dec d) (cons d xs))
                :else             (recur (dec d) xs)))]
      (f (inc (quot n 2)) (list n))))

(defn count-divisors [n]
  (->> (prime-factors-of n)
       frequencies
       vals
       (map inc)
       (reduce *)))

(defn smallest-dividee [num-divisors]
  (let [f (fn [a b] (math/expt a (dec b)))]
    (reduce * (map f primes (reverse (prime-factors-of num-divisors))))))

(defmacro fmt [^String string]
  (let [-re #"#\{(.*?)\}"
        fstr (clojure.string/replace string -re "%s")
        fargs (map #(read-string (second %)) (re-seq -re string))]
    `(format ~fstr ~@fargs)))

(defn sigma [k n]
  (letfn [(term [entry]
            (let [p (key entry)
                  e (val entry)]
              (cond 
                (= n 1)   1
                (zero? k) (inc e) 
                :else     (quot 
                            (dec (math/expt p (* (inc e) k)))
                            (dec (math/expt p k))))))]
    (->> (prime-factors-of n) frequencies (map term) (reduce *))))

(defn aliquot-sum [n]
  (- (sigma 1 n) n))

(defn quadratic-root
  [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      []
      (let [sqr (math/sqrt discriminant)
            deno (* 2 a)]
        [(/ (+ (- b) sqr) deno) (/ (- (- b) sqr) deno)]))))

(defn quadratic-root-pred?
  [pred a b c]
  (any? pred (quadratic-root a b c)))

(defn is-hexagonal?
  [x]
  (quadratic-root-pred? pos-int? 2 -1 (- x)))

(defn expand-continued-fraction [n]
  (let [a0 (int (Math/sqrt n))]
    (loop [m 0, d 1, a a0, acc [a0]]
      (if (= a (* 2 a0))
        acc
        (let [m (- (* d a) m), d (/ (- n (* m m)) d), a (int (/ (+ a0 m) d))]
          (recur m d a (conj acc a)))))))