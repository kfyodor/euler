(ns euler.p4)

(defn palindrome?
  [n]
  (let [n-str (str n)]
    (= n-str (clojure.string/reverse n-str))))

(defn make-palindrome-products
  "Makes a set of palindrome products."
  [n1 n2]
  (loop [result (sorted-set) n n1]
    (let [nums     (range n (inc n2))
          products (filter palindrome?
                           (map #(* n %) nums))
          _result (if (empty? products)
                    result
                    (apply conj result products))]
      (if (= n n2)
        _result
        (recur _result (inc n))))))

(defn solve []
  (let [_min 100
        _max 999
        products (make-palindrome-products _min _max)]
    (last products)))
