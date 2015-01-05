(ns euler.p2)

(defn fib-list []
  (fn fib-seq [n-2 n-1]
    (cons n-2 (lazy-seq
                (fib-seq n-1
                         (+' n-1 n-2)))))
  (fib-seq 1 2))

(prn (reduce +
            (filter #(even? %)
                    (take-while
                      #(< % 4000000)
                      (fib-list)))))
