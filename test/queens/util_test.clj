(ns queens.util-test
    (:use clojure.test
    	  queens.test-util
          queens.util))

(deftest coll-pred-test
    (testing "Should return size of list filtered by argument predicate for second argument value"
        (is (coll-pred [1 2 3] #(= % 1) ))
        (is (coll-pred [1 2 3] #(= % 2) ))
        (is (coll-pred [1 2 3] #(= % 3) ))
        (is (not (coll-pred [1 2 3] #(= % 4) )))
        (is (not (coll-pred [1 2 3] #(= % 5) )))
        (is (coll-pred [1 2 3] #(< % 10) ))
        (is (not (coll-pred [1 2 3] #(< % 1) )))))
        
(deftest id-generator-test
    (testing "Should return a function which increments an id atomically and returns a new id on demand" 
        (let [nextId (id-generator)]
            (is (= 1 (nextId)))
            (is (= 2 (nextId)))
            (is (= 3 (nextId))))))
                    
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
		(is (not (contains-cell? [] [])))))		
		
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
		
(deftest gcd-test
    (testing "Should return the gcd of two integers"
        (is (= 4 (gcd 8 12)))
        (is (= 2 (gcd 4 6)))
        (is (= 1 (gcd 7 6)))
        (is (= 3 (gcd 9 6)))
        (is (= 1 (gcd 2 7)))
        (is (= 1 (gcd 90 91)))))

(deftest gcd-test-largenums
    (testing "Should return the gcd of large numbers w/o running out of stack"
        (is (= 2 (gcd 45678342 763514)))
        (is (= 2 (gcd 982 76)))
        (is (= 5 (gcd 135 8675)))
        (is (= 1 (gcd 13894 17)))))
                
        
(deftest smallest-slope-diff-test
	(testing "Should return the distance b/w respective coordinates of two neighbouring cells on a sloped line"
		(is (= [1 1] (smallest-slope-diff [2 2] [7 7])))
		(is (= [2 3] (smallest-slope-diff [3 2] [7 8])))
		(is (= [2 3] (smallest-slope-diff [7 8] [3 2])))
		(is (= [7 11] (smallest-slope-diff [4 2] [11 13])))
		(is (= [3 2] (smallest-slope-diff [1 1] [13 9])))
		(is (= [3 2] (smallest-slope-diff [1 1] [10 7])))
		(is (= [3 2] (smallest-slope-diff [1 1] [7 5])))
		(is (= [3 2] (smallest-slope-diff [1 1] [4 3])))
		(is (= [3 2] (smallest-slope-diff [4 3] [1 1])))
		(is (= [3 2] (smallest-slope-diff [7 5] [1 1])))
		(is (= [3 2] (smallest-slope-diff [4 3] [13 9])))
		(is (= [3 2] (smallest-slope-diff [7 5] [13 9])))
		(is (= [3 2] (smallest-slope-diff [10 7][13 9])))
		(is (= [3 2] (smallest-slope-diff [10 7][7 5])))
		
		(is (not (= [2 2] (smallest-slope-diff [10 7][7 5]))))
		(is (not (= [1 1] (smallest-slope-diff [3 2] [7 8]))))
		))
		
(deftest smallest-increments-between-test
	(testing "Should return the distance b/w respective coordinates of two neighbouring cells on any line, including vertical/horizontal"
		(is (= [0 1] (smallest-increments-between [4 2] [4 3])))
		
		(is (= [1 1] (smallest-increments-between [2 2] [7 7])))
		(is (= [2 3] (smallest-increments-between [3 2] [7 8])))
		(is (= [2 3] (smallest-increments-between [7 8] [3 2])))
		(is (= [7 11] (smallest-increments-between [4 2] [11 13])))))
		
(deftest outside?-test
	(testing "Should return true iff cell is outside of a grid with given dimentsions"
		(is (outside? [1 5] 3))
		(is (outside? [-1 2] 3))
		(is (outside? [0 1] 3))
		(is (not (outside? [3 45] 77)))))		
		
        
(deftest line-forward-test
	(testing "Should return a full line-segment from starting cell to the border or closest cell nearof, going forward"
		(is (= [[7 5][ 10 7][13 9]] (line-forward [3 2] [7 5] 13)))
                (is (= [[3 7][7 9][11 11]] (line-forward [4 2] [3 7] 13)))
                (is (= [[7 9][11 11]] (line-forward [4 2] [7 9] 13))) 
                (is (= [[11 11]]      (line-forward [4 2] [11 11] 13)))
                (is (= [[3 7][5 6][7 5][9 4][11 3][13 2]] (line-forward [2 -1] [3 7] 13)))
                (is (= [[1 8][3 7][5 6][7 5][9 4][11 3][13 2]] (line-forward [2 -1] [1 8] 13)))
                (is (= [[1 9][1 10][1 11][1 12][1 13]] (line-forward [0 1] [1 9] 13)))
                (is (= [[9 6][10 6][11 6][ 12 6][13 6]] (line-forward [1 0] [9 6] 13)))
                (is (= [[1 2][2 3]] (line-forward [1 1] [1 2] 3)))
                (is (= [[1 3][2 2][3 1]] (line-forward [1 -1] [1 3] 3)))
                (is (= [[1 2][8 1]] (line-forward [7 -1] [1 2] 13)))))

(deftest inverse-test
        (testing "Should return the sign-opposite of a vector of deltas"
                (is (= [1 -1] (inverse [-1 1])))
                (is (= [3 7] (inverse [-3 -7])))))

(deftest line-backward-test
        (testing "Should return a full line-segment from starting cell to the border or closest cell nearof, backward"
                (is (= [[1 1][2 3]] (line-backward [1 2] [2 3] 3)))
                (is (= [[3 7][7 9]] (line-backward [4 2] [7 9] 13)))
                (is (= [[3 7][7 9][11 11]] (line-backward [4 2] [11 11] 13)))
                (is (= [[3 10][7 9]] (line-backward [4 -1] [7 9] 13)))
                (is (= [[3 10][7 9][11 8]] (line-backward [4 -1] [11 8] 13)))
                (is (= [[6 1][7 13]] (line-backward [1 12] [7 13] 13)))
                (is (= [[1 1][1 2][1 3][1 4]] (line-backward [0 1] [1 4] 13)))
                (is (= [[1 5][2 5][3 5][4 5][5 5]] (line-backward [1 0] [5 5] 13)))
                (is (= [[1 10][2 9][3 8]] (line-backward [1 -1] [3 8] 13)))))

(deftest full-line-test
        (testing "Should return a full line in row/col order"
                (is (= [[1 1][2 2][3 3]] (full-line [1 1] [1 1] 3)))
                (is (= [[1 1][2 2][3 3]] (full-line [1 1] [2 2] 3)))
                (is (= [[1 4][5 5][9 6][13 7]] (full-line [4 1] [5 5] 13)))
                (is (= [[1 4][5 5][9 6][13 7]] (full-line [4 1] [13 7] 13)))
                (is (= [[1 4][5 5][9 6][13 7]] (full-line [4 1] [1 4] 13)))
                (is (= [[8 2][5 4][2 6]] (full-line [-3 2] [5 4] 13)))
                (is (= [[8 2][5 4][2 6]] (full-line [-3 2] [2 6] 13)))
                (is (= [[8 2][5 4][2 6]] (full-line [-3 2] [8 2] 13)))
                (is (= [[2 1][2 2][2 3][2 4][2 5]] (full-line [0 1] [2 4] 5)))
                (is (= [[1 4][2 4][3 4][4 4]] (full-line [1 0] [3 4] 4)))
                (is (= [[1 4][2 4][3 4][4 4]] (full-line [1 0] [4 4] 4)))))

(deftest line-test
        (testing "Should return a full line using only two cells and size inputs"
            (is (= [[1 3][2 1]]) (line [1 3] [2 1] 3))
            (is (= [[1 3][2 1]]) (line [2 1] [1 3] 3))
            (is (= [[1 2][4 4][7 6][10 8][13 10]]) (line [7 6] [13 10] 13))            
            (is (= [[1 2][4 4][7 6][10 8][13 10]]) (line [13 10] [7 6] 13))
            (is (= [[1 2][4 4][7 6][10 8][13 10]]) (line [4 4] [10 8] 13))
            (is (= [[1 2][4 4][7 6][10 8][13 10]]) (line [1 2] [7 6] 13))
            (is (= [[1 2][4 4][7 6][10 8][13 10]]) (line [10 8] [7 6] 13)) 
            (is (= [[1 2][4 4][7 6][10 8][13 10]]) (line [10 8] [1 2] 13)) 
            (is (= [[2 7][6 4][10 1]]) (line [2 7] [10 1] 13))
            (is (= [[2 7][6 4][10 1]]) (line [6 4] [2 7] 13))
            (is (= [[2 7][6 4][10 1]]) (line [10 1] [2 7] 13))))

(deftest filter-baselines-test
        (testing "Should return only cells not forming a  baseline"
            (is (= [[5 4][7 6]] (filter-baselines [[1 1][4 2][5 4][7 6]] [3 3])))
            (is (= [[1 1][4 2]] (filter-baselines [[1 1][4 2][5 4][7 6]] [9 4])))
            (is (= [[1 1][4 2][5 4][7 6]]  (filter-baselines [[1 1][4 2][5 4][7 6]] [9 5])))
            (is (= [[7 6]]  (filter-baselines [[1 1][4 2][5 4][7 6]] [4 4])))))

(deftest irregular-lines-test
        (testing "Should return all complete lines which are irregular"
            (is (= [[[1 1][4 2][7 3]] [[4 2][5 4][6 6][7 8]]] (irregular-lines [[1 1] [5 4]] [4 2] 9)))
            (is (= [[[1 7][3 6][5 5][7 4][9 3]] [[4 2][5 5][6 8]]] (irregular-lines [[3 6][6 8]] [5 5] 9)))))

(deftest any-line?-test
        (testing "Should tell whether a cell is within any lines from a collection of lines"
            (let [ lines  [[[1 1][4 2][7 3]] [[4 2][5 4][6 6][7 8]]] ]
            (is (any-line? lines [6 6]))
            (is (any-line? lines [4 2]))
            (is (any-line? lines [7 3]))
            (is (any-line? lines [1 1]))
            (is (any-line? lines [7 8]))
            (is (not (any-line? lines [3 3])))
            (is (not (any-line? lines [45 0]))))))
