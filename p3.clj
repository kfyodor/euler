(ns euler.p3
  [:require [clojure.set :refer :all]])

(defn sieve-map
  "Lazy implementation of Sieve of Eratosthenes.
   Implemented with Map as a storage.
   See http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
   for more information."
  ([]
    (primes 2 {}))
  ([n storage]
   (lazy-seq
     (let [next (inc n)]
       (if (contains? storage n)
         (let [factors      (storage n)
               next-factors (zipmap (map #(+ % n)   factors)
                                    (map #(vec [%]) factors))
               merge-fn     (fn [a b] (vec (concat a b)))
               new-storage  (merge-with merge-fn
                                        (dissoc storage n)
                                        next-factors)]
           (primes next new-storage))
         ; else
         (cons n (primes next
                         (assoc storage
                                (* n n)
                                (vec [n])))))))))

(defn primes-trial [n]
  (let [prime? (fn [_n] (let [num (int (Math/sqrt _n))]
                         (loop [i 2]
                           (cond
                            (= _n 1) false
                            (> i num) true
                            (zero? (rem _n i)) false
                            :other (recur (inc i))))))]
    (take-while #(<= % (Math/sqrt n)) (filter prime? (iterate inc 2)))))

;; (defn sieve-pq
;;   "Same as sieve-map but with PriorityQueue as a storage."
;;   ([])
;;   ([]))

(defn factorize
  [n]
  "Integer factorization via the trial division algorithm."
  (filter #(= 0 (mod n %))
          (take-while #(< % (Math/sqrt n))
                      (sieve-map))))

(defn factorize-trial
  [n]
  (filter #(= 0 (mod n %)) (primes-trial n)))

(defn largest-factor-trial [n]
  "Computer largest prime factor of a given number
   via the trial division algorithm."
  (last (factorize n)))
