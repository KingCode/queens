(ns queens.core-test
  (:use clojure.test
        queens.core))

;;define a test with automated global var overriding
(defmacro def-btest [test-name size queens lines & body]
    `(deftest ~test-name
        (binding [state (atom {
                                :queens ~queens
                                :size ~size
                                :lines ~lines
                          })]
                     (do ~@body))))

(deftest coll-pred-test
    (testing "Should return size of list filtered by argument predicate for second argument value"
        (is (coll-pred [1 2 3] #(= % 1) ))
        (is (coll-pred [1 2 3] #(= % 2) ))
        (is (coll-pred [1 2 3] #(= % 3) ))
        (is (not (coll-pred [1 2 3] #(= % 4) )))
        (is (not (coll-pred [1 2 3] #(= % 5) )))
        (is (coll-pred [1 2 3] #(< % 10) ))
        (is (not (coll-pred [1 2 3] #(< % 1) )))))
        
(deftest same?-test
	(testing "Should return true iff two cells have equal coordinates, and handle nulls/empties with false returns"
		(is (same? [1 1] [ 1 1]))
		(is (same? [2 3] [2 3]))
		(is (not (same? [4 5] [5 4])))
		(is (not (same? [] [1 2])))
		(is (not (same? [4 6] [])))
		(is (not (same? nil [1 2])))
		(is (not (same? [1 1] nil)))
		(is (not (same? nil nil)))))
		
(deftest contains-cell?-test
	(testing "Should return true when a cell is found, and false otherwise, and handle nulls/empties with false returns"
		(is (contains-cell? [[1 2] [3 4]] [1 2]))
		(is (contains-cell? [[1 2] [3 4] [5 6]] [3 4]))
		(is (contains-cell? [[1 2] [3 4] [5 6]] [5 6]))
		(is (not (contains-cell? [[1 2] [3 4]] [5 6])))
		(is (not (contains-cell? [[1 2] [3 4]] [2 1])))
		(is (not (contains-cell? [] [1 1])))
		(is (not (contains-cell? [[1 1]] nil)))
		(is (not (contains-cell? nil [ 1 1])))
		(is (not (contains-cell? [] [1 2])))
		(is (not (contains-cell? [] nil)))
		(is (not (contains-cell? nil nil)))
		(is (not (contains-cell? nil [1 1])))
		(is (not (contains-cell? [] [])))		
))		

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

(deftest same-row-test 
    (testing "Should return truee if two cells are on the same row"
        (is (same-row? [1 1] [1 2]))
        (is (same-row? [1 1] [1 3]))
        (is (same-row? [2 1] [2 5]))
        (is (same-row? [3 3] [3 3]))
        (is (not (same-row? [1 1] [2 5])))
        (is (not (same-row? [1 1] [2 1])))))

(deftest same-col-test
    (testing "Should return true iff two cells are on the same column"
        (is (same-col? [1 1] [2 1]))
        (is (same-col? [3 3] [1 3]))
        (is (same-col? [5 5] [2 5]))
        (is (not (same-col? [1 1] [1 2])))
        (is (not (same-col? [2 3] [3 2])))))

(deftest same-diag-test
    (testing "Should return true iff two cells are on the same 45 degree sloped diagonal"
        (is (same-diag? [1 1] [2 2]))
        (is (same-diag? [1 3] [2 2]))
        (is (same-diag? [1 5] [5 1]))
        (is (same-diag? [1 2] [2 3]))
        (is (same-diag? [1 2] [3 4]))
        (is (same-diag? [1 2] [4 5]))
        (is (same-diag? [3 4] [4 5]))
        (is (same-diag? [4 5] [2 3]))
        (is (not (same-diag? [1 1] [1 2])))
        (is (not (same-diag? [2 2] [3 2])))
        (is (not (same-diag? [1 1] [4 5])))
        (is (not (same-diag? [2 5] [3 1])))))
        
(deftest same-baseline-test
    (testing "Should return true iff two cells are on the same row, column or 45 degree angle diagonal"
        (is (same-baseline? [1 1] [1 4]))
        (is (same-baseline? [1 1] [4 1]))
        (is (same-baseline? [1 1] [5 5]))
        (is (same-baseline? [2 4] [4 4]))
        (is (same-baseline? [2 4] [2 1]))
        (is (same-baseline? [2 4] [5 1]))
        (is (not (same-baseline? [1 1] [5 2])))
        (is (not (same-baseline? [1 1] [2 3])))
        (is (not (same-baseline? [3 3] [5 4])))))

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

(def-btest add-queen-test 4 [] {}
    (testing "Should add a queen to existing ones"
        (add-queen [1 2])
        (is (= [[1 2]] (:queens @state)))
        (add-queen [3 4])
        (add-queen [2 2])
        (is (= [[1 2] [3 4] [2 2]]))))

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

(def-btest all-lines-with-test 5 [] {}
	(testing "Should draw lines with endings on grid border, each defined by an element in a sorted collection and a common cell"
		(is (= [ [[1 1] [1 2] [1 3] [1 4] [1 5]]] [[1 1]] [1 5]))
))
        
(deftest id-generator-test
    (testing "Should return a function which increments an id atomically and returns a new id on demand" 
        (let [nextId (id-generator)]
            (is (= 1 (nextId)))
            (is (= 2 (nextId)))
            (is (= 3 (nextId))))))

            
(def-btest verify-test-withargs 11 [] {}
    (testing "Should verify a compliant candidate solution and invalidate others"
        (is (verify [] [[1 1]] []))
        (is (verify [[1 1]] [[3 2]] []))
        (is (verify [] [[1 1] [3 2]] []))
        (is (verify [] [[1 1] [3 2] [5 3] [7 4] [9 5]] []))
        (is (not (verify [[1 1]] [[2 2]] [])))
        (is (not (verify [[1 1]] [[2 1]] [])))
        (is (not (verify [[1 1]] [[5 1]] [])))
        (is (not (verify [[1 1]] [[1 11]] [])))
        (is (not (verify [] [[1 1] [2 2] [3 2] [5 5]] [])))
))      

(def-btest verify-test-1 11 [[1 1] [3 2] [5 3] [7 4] [9 5]] {}
	(testing "Should verify that the current state is a valid solution, even though incomplete"
		(is (verify))
))		
	  
;;2 4 7 1 8 11 5 3 9 6 10
(def-btest verify-test-2 11 [[1 2] [2 4] [3 7] [4 1] [5 8] [6 11] [7 5] [8 3] [9 9] [10 6] [11 10]] {}
	(testing "Should verify that the current state is a valid solution (size 11)"
		(is (verify))))

;;1 3 12 10 7 2 11 5 8 13 9 4 6
(def-btest verify-test-3 13 [ [1 1] [2 3] [3 12] [4 10] [5 7] [6 2] [7 11] [8 5] [9 8] [10 13] [11 9] [12 4] [13 6]] {}
	(testing "Should verify that the current state is a valid solution (size 13)"
		(is (verify))))

;;1 3 5 7 2 4 6
(def-btest verify-test-4 7 [ [1 1] [2 3] [3 5] [4 7] [5 2] [6 4] [7 6]] {}
	(testing "Should invalidate the current state if not compliant (size 7)"
		(is (not (verify)))))

        

