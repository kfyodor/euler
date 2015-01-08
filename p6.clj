(ns euler.p6)

(defn solve-naive [n]
  "Straightforward approach"
  (let [sum-of-squares (reduce + (map #(* % %) (range 1 (inc n))))
        sum            (reduce + (range 1 (inc n)))
        square-of-sum  (* sum sum)]
    (- square-of-sum sum-of-squares)))

(defn solve [n]
  "http://en.wikipedia.org/wiki/Square_number
   http://mathcentral.uregina.ca/QQ/database/QQ.02.06/jo1.html"
  (let [sum-of-squares (/ (* n (inc n) (inc (* 2 n))) 6)
        sum            (/ (* n (inc n)) 2)
        square-of-sum  (* sum sum)]
    (- square-of-sum sum-of-squares)))
