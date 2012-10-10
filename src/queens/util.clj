(ns queens.util)

;;    \gcd(a,a) = a
;;    \gcd(a,b) = \gcd(a - b,b)\quad, if b<a
;;    \gcd(a,b) = \gcd(a, b-a)\quad,

(defn gcd_op [a b] 
    (cond (= a b) a
          (< b a) #(gcd_op (- a b) b)
          :else   #(gcd_op a (- b a))))

(defn gcd [a b] (trampoline (gcd_op a b)))

