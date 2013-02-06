(ns queens.core-test
  (:use clojure.test
	queens.test-util
        queens.core
        queens.cache
        queens.state))

(def-btest occupied-test 5 [[1 1] [2 4] [3 2] [4 5] [5 3]] {}
    (testing "Should return true iff cell is occupied"
        (is (occupied? [1 1]))
        (is (occupied? [5 3]))
        (is (occupied? [2 4]))
        (is (occupied? [4 5]))
        (is (occupied? [3 2]))
        (is (not (occupied? [1 2])))
        (is (not (occupied? [1 3])))
        (is (not (occupied? [2 1])))
        (is (not (occupied? [3 3])))))


(def-btest outside-boundary-test 3 [] {}
    (testing "Should return true iff a cell is outside the grid boundary of 3"
        (is (outside-boundary? [1 4]))
        (is (outside-boundary? [0 1]))
        (is (outside-boundary? [-1 2]))
        (is (outside-boundary? [5 1]))
        (is (outside-boundary? [5 -1]))
        (is (outside-boundary? [-9 -3]))
        (is (not (outside-boundary? [1 2])))
        (is (not (outside-boundary? [2 2])))
        (is (not (outside-boundary? [3 3])))))

(def-btest line-with-test 11 [] {}
    (testing "Should return all cells in the grid for the line containing the two argument cells sorted by scanning order"
        (is (= [[1 1] [3 2] [5 3] [7 4] [9 5] [11 6]] (line-with [1 1] [3 2])))
        (is (= [[2 4] [5 3] [8 2] [11 1]] (line-with [2 4] [5 3])))
        (is (= [[1 4] [3 3] [5 2] [7 1]] (line-with [1 4] [3 3])))
        (is (= [[4 1] [5 11]] (line-with [4 1] [5 11])))
        (is (= [[3 2] [6 3] [9 4]] (line-with [3 2] [6 3])))
        (is (= [[1 11] [5 6] [9 1]] (line-with [1 11] [5 6])))
        (is (= [[2 10] [3 8] [4 6] [5 4] [6 2]] (line-with [2 10] [3 8])))))


(def-btest same-baseline-any-test 11 [[1 1] [3 2] [5 3] [7 4] [9 5]] {}
    (testing "Should detect whether a cell is on the same baseline as existing queens"
        (is (same-baseline-any? [2 1]))
        (is (same-baseline-any? [3 3]))
        (is (same-baseline-any? [6 4]))
        (is (same-baseline-any? [6 3]))
        (is (same-baseline-any? [1 4]))
        (is (same-baseline-any? [9 9]))
        (is (same-baseline-any? [7 5]))
        (is (not (same-baseline-any? [11 6])))
        (is (not (same-baseline-any? [10 11])))
        (is (same-baseline-any? [8 7]))
        (is (not (same-baseline-any? [6 10])))))

(comment
(def-btest lines-between-test 5 [] {}
	(testing "Should draw lines with endings on grid border, each defined by an element in a sorted collection and a common cell"
		(is (= [ [[1 1] [1 2] [1 3] [1 4] [1 5]]] (lines-between [[1 1]] [1 5]))
)))     )   

            
(init-cache-and-test verify-test-withargs 11 [] {}
    (testing "Should verify a compliant candidate solution and invalidate others"
        (is (verify [] [[1 1]] []))
        (is (verify [[1 1]] [[3 2]] []))
        (is (verify [] [[1 1] [3 2]] []))
        (is (not (verify [] [[1 1] [3 2] [5 3] [7 4] [9 5]] [])))
        (is (not (verify [[1 1]] [[2 2]] [])))
        (is (not (verify [[1 1]] [[2 1]] [])))
        (is (not (verify [[1 1]] [[5 1]] [])))
        (is (not (verify [[1 1]] [[1 11]] [])))
        (is (not (verify [] [[1 1] [2 2] [3 2] [5 5]] [])))
))      

(init-cache-and-test verify-test-1 11 
	(testing "Should verify that the current state is a valid solution, even though incomplete"
	  (let [ qs [[1 1] [3 2] [5 3] [7 4] [9 5]] ]
		(is (not (verify qs))))
))		
	  
;;2 4 7 1 8 11 5 3 9 6 10
(init-cache-and-test verify-test-2 11 
	(testing "Should verify that the current state is a valid solution (size 11)"		
	  (let [ qs [[1 2] [2 4] [3 7] [4 1] [5 8] [6 11] [7 5] [8 3] [9 9] [10 6] [11 10]] ]
		(is (verify qs)))))

;;1 3 12 10 7 2 11 5 8 13 9 4 6
(init-cache-and-test verify-test-3 13 
	(testing "Should verify that the current state is a valid solution (size 13)"
	  (let [qs [ [1 1] [2 3] [3 12] [4 10] [5 7] [6 2] [7 11] [8 5] [9 8] [10 13] [11 9] [12 4] [13 6]] ]
		(is (verify qs)))))

;;1 3 5 7 2 4 6
(init-cache-and-test verify-test-4 7 
	(testing "Should invalidate the current state if not compliant (size 7)"
	  (let [ qs [ [1 1] [2 3] [3 5] [4 7] [5 2] [6 4] [7 6]] ]
		(is (not (verify qs))))))

;;4 2 5 3 1
(init-cache-and-test verify-test-5 5 
	(testing "Should invalidate a non compliant solution (size 5)"
	  (let [ qs [ [1 4] [2 2] [3 5] [4 3] [5 1]] ]
		(is (not (verify qs))))))

;;4 2 5 3 (remove last cell from previous)
(init-cache-and-test verify-test-6 5 
        (testing "Should validate a compliant collection, even tough not a complete solution (size 5)"
          (let [ qs [ [1 4] [2 2] [3 5] [4 3]] ]
            (is (verify qs)))))


;;4 2 3 1 (remove first of last three cells from verify-test-5)
(init-cache-and-test verify-test-7 5 
        (testing "Should validate a compliant collection, even though not a complete solution (size 5)"
          (let [ qs [ [1 4] [2 2] [4 3] [5 1]] ]
            (is (verify qs)))))

(init-cache-and-test inc-set-test 3
    (testing "Should yield all incremental partial solutions for current queens, size 3"
        (let [ q1 '([1 1])
               exp1 '(([1 1] [2 3]))
               act1 (inc-set q1)
                           
               q2 '([1 1] [2 3])
               exp2 '()
               act2 (inc-set q2)
               
               q2 '([1 3])
               exp2 '(([1 3][2 1]))
               act2 (inc-set q2)               
              
               q3 '([2 2])
               exp3 '()
               act3 (inc-set q3)
              ]
              
            (is (= exp1 act1))            
            (is (= exp2 act2))
            (is (= exp3 act3))
)))


(init-cache-and-test inc-set-test2 4
    (testing "Should yield all incremental partial solutions for current queens, size 4"
        (let [ q1 '([1 1])
               exp1 '(([1 1][2 3]) ([1 1][2 4]))
               act1 (inc-set q1)
               
               q2 '([1 1][2 3])
               exp2 '()
               act2 (inc-set q2)
               
               q3 '([1 1][2 4])
               exp3 '(([1 1][2 4][3 2]))
               act3 (inc-set q3)
               
               q4 '([1 1][2 4][3 2])
               exp4 '()
               act4 (inc-set q4)
               
               q5 '([1 2])
               exp5 '(([1 2][2 4]))
               act5 (inc-set q5)
               
               q6 '([1 2][2 4])
               exp6 '(([1 2][2 4][3 1]))
               act6 (inc-set q6)
               
               q7 '([1 2][2 4][3 1])
               exp7 '(([1 2][2 4][3 1][4 3]))
               act7 (inc-set q7)               
            ]
        (is (= exp1 act1))
        (is (= exp2 act2))
        (is (= exp3 act3))
        (is (= exp4 act4))
        (is (= exp5 act5))
        (is (= exp6 act6))
        (is (= exp7 act7))
)))
