(ns euler.p7)

;; Taken from Problem 3 solution

(defn sieve-map
  "Lazy implementation of Sieve of Eratosthenes.
   Implemented with Map as a storage.
   See http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
   for more information."
  ([]
    (sieve-map 2 {}))
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
           (sieve-map next new-storage))
         ; else
         (cons n (sieve-map next
                         (assoc storage
                                (* n n)
                                (vec [n])))))))))

(defn solve [n]
  (last (take n (sieve-map))))
