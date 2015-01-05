(ns euler.p5)

(defn gcd
  "Computes greatest common divisor
   for given numbers"
  [n1 n2]
  (loop [a n1 b n2]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn lcm
  "Computes least common multiple
   for given numbers"
  [n1 n2]
  (/ (* n1 n2) (gcd n1 n2)))

(defn solve
  []
  (reduce lcm (range 1 21)))
