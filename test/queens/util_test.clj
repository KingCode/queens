(ns queens.util-test
    (:use clojure.test
          queens.util))

(deftest gcd-test
    (testing "Should return the gcd of two integers"
        (is (= 4 (gcd 8 12)))
        (is (= 2 (gcd 4 6)))
        (is (= 1 (gcd 7 6)))
        (is (= 3 (gcd 9 6)))
        (is (= 1 (gcd 90 91)))))


(deftest gcd-test-largenums
    (testing "Should return the gcd of large numbers w/o running out of stack"
        (is (= 2 (gcd 45678342 763514)))
        (is (= 2 (gcd 982 76)))
        (is (= 5 (gcd 135 8675)))
        (is (= 1 (gcd 13894 17)))))

        
