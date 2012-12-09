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

(deftest in-baseline-test
    (testing "Should return all cells in same baseline as candidate cell"
        (is (= [[1 1] [5 7] [33 6]] (in-baseline [[1 1] [5 7] [10 9] [33 6]] [6 6])))
        (is (= [] (in-baseline [[1 4] [2 2] [3 5] [4 3]] [6 7])))))
		
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
        	(is (= [[1 1][1 2][1 3]] (line [1 1] [1 3] 3)))
        	(is (= [[1 1][1 2][1 3]] (line [1 1] [1 2] 3)))
        	(is (= [[1 2][2 2][3 2]] (line [2 2] [3 2] 3)))
        	(is (= [[1 2][2 2][3 2]] (line [1 2] [3 2] 3)))
        	(is (= [[2 1][3 2][4 3][5 4]] (line [3 2] [5 4] 5)))
        	(is (= [[2 1][3 2][4 3][5 4]] (line [5 4] [3 2] 5)))
        	(is (= [[2 1][3 2][4 3][5 4]] (line [4 3] [2 1] 5)))
            (is (= [[1 3][2 1]] (line [1 3] [2 1] 3)))
            (is (= [[1 3][2 1]] (line [2 1] [1 3] 3)))
            (is (= [[1 2][4 4][7 6][10 8][13 10]] (line [7 6] [13 10] 13)))
            (is (= [[1 2][4 4][7 6][10 8][13 10]] (line [13 10] [7 6] 13)))
            (is (= [[1 2][4 4][7 6][10 8][13 10]] (line [4 4] [10 8] 13)))
            (is (= [[1 2][4 4][7 6][10 8][13 10]] (line [1 2] [7 6] 13)))
            (is (= [[1 2][4 4][7 6][10 8][13 10]] (line [10 8] [7 6] 13)))
            (is (= [[1 2][4 4][7 6][10 8][13 10]] (line [10 8] [1 2] 13)))
            (is (= [[2 7][6 4][10 1]] (line [2 7] [10 1] 13)))
            (is (= [[2 7][6 4][10 1]] (line [6 4] [2 7] 13)))
            (is (= [[2 7][6 4][10 1]] (line [10 1] [2 7] 13)))))

(deftest filter-baselines-test
        (testing "Should return only cells not forming a  baseline"
            (is (= [[5 4][7 6]] (filter-baselines [[1 1][4 2][5 4][7 6]] [3 3])))
            (is (= [[1 1][4 2]] (filter-baselines [[1 1][4 2][5 4][7 6]] [9 4])))
            (is (= [[1 1][4 2][5 4][7 6]]  (filter-baselines [[1 1][4 2][5 4][7 6]] [9 5])))
            (is (= [[7 6]]  (filter-baselines [[1 1][4 2][5 4][7 6]] [4 4])))))

(deftest irregular-lines-test
        (testing "Should return all complete lines formed between a candidate and any coll. elements, which are irregular"
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

(deftest query-cells-with-test
        (testing "Should show all occupied cells participating in a line with a candidate cell"
            (let [ lines-0 (irregular-lines [[1 4]] [2 2] 7)
                   lines-1 (irregular-lines [[1 4][2 2]] [3 5] 7)
                   lines-2 (irregular-lines [[1 4][2 2][3 5]] [4 3] 7)
                   lines (vec (set (concat lines-0 lines-1 lines-2)))
                   coll [[1 4][2 2][3 5][4 3]] 
                   c-1 [6 6]
                   c-2 [2 7]
                   c-3 [2 4]
                   c-4 [5 1]
                   c-5 [6 4]
                   c-6 [5 6]
                   c-7 [6 7]
                 ]
                
                 (is (= [] (query-cells-with coll lines nil)))
                 (is (= [] (query-cells-with [] lines c-1)))
                 (is (= [[2 2]] (query-cells-with coll lines c-1)))
                 (is (= [[2 2] [[3 5][4 3]]] (query-cells-with coll lines c-2)))
                 (is (= [[1 4] [2 2] [3 5]] (query-cells-with coll lines c-3)))
                 (is (= [[[3 5][4 3]]] (query-cells-with coll lines c-4)))
                 (is (= [[1 4] [[2 2][4 3]]] (query-cells-with coll lines c-5)))
                 (is (= [[[1 4][3 5]]] (query-cells-with coll lines c-6)))
                 (is (= [] (query-cells-with coll lines c-7))))))

(deftest query-cells-with-test-morelines
        (testing "Should show all occupied cells participating in a line with a candidate cell"
            (let [ lines-0 (irregular-lines [[1 4]] [2 2] 13)
                   lines-1 (irregular-lines [[1 4][2 2]] [3 5] 13)
                   lines-2 (irregular-lines [[1 4][2 2][3 5]] [4 3] 13)
                   lines-3 (irregular-lines [[1 4][2 2][3 5][4 3]] [6 7] 13)
                   lines-4 (irregular-lines [[1 4][2 2][3 5][4 3][6 7]] [9 6] 13)
                   lines (vec (set (concat lines-0 lines-1 lines-2 lines-3 lines-4)))
                   coll [[1 4][2 2][3 5][4 3][6 7][9 6]] 
                   c-1 [12 5]
                ]
                (is (= [[3 5] [[6 7][9 6]]] (query-cells-with coll lines c-1))))))

(deftest move-1-test
		(testing "Should move one position forward in row/col order taking limit into account"
			(is (= [1 2] (move-1 [1 1] 3)))
			(is (= [2 1] (move-1 [1 3] 3)))
			(is (= [3 3] (move-1 [3 2] 3)))
			(is (= nil 	 (move-1 [3 3] 3)))
))			


(deftest compare-test
		(testing "Should compare two positions according to java comparator semantics using row/col order"
			(is (before? [1 1] [1 2]))
			(is (not (before? [1 2] [1 1])))
			(is (before? [24 10] [25 1]))
			(is (after? [1 2] [1 1]))
			(is (after? [2 1] [1 1000]))))
			
			
(deftest search-test
		(testing "Should return the zero-based index of a cell if found, or zero"
			(is (= 0 (search [[1 1] [2 1]] [1 1])))
))						
	                
(deftest found?-test
      (testing "Should tell quickly whether a cell is in a large collection"
            (let [ coll [[1 3] [2 4] [5 6] [6 10] [8 3]]  
                        
            	   bigend 51  ;; Setting this to 200 caused large colls' initialization 
            	   			   ;; to take several minutes to complete on my PC
            	   diffsiz 20  ;; MUST be less than 'bigend'
            	   lessend (- bigend diffsiz)
            	   big (apply vec #{(for [ x (range 1 bigend) y (range 1 bigend) ] [x y])})
            	   diff (apply vec #{ (for [ x (range 1 (inc diffsiz)) y (range 1 (inc diffsiz)) ] 
            	   							[ (inc (rand-int diffsiz)) (inc (rand-int diffsiz)) ])})
            	   less (apply vec #{ (for [ x (range 1 lessend) y (range 1 lessend)
            	   						:when (not-in? diff [x y]) ] [x y])})
            	   						
				   c1 (nth less 0)
				   c2 (nth less (quot lessend 2))
				   c3 (nth less (dec lessend))
				   c4 (nth diff 0)
				   c5 (nth diff (quot diffsiz 2))
				   c6 (nth diff diffsiz)
            ]                
                (is (found? coll [5 6]))
                (is (not (found? coll nil)))
                (is (not (found? nil [1 3])))
                (is (not (found? nil nil)))
                (is (found? less c1))                
                (is (found? less c2))
                (is (found? less c3))
                (is (not (found? less c4)))
                (is (not (found? less c5)))
                (is (not (found? less c6)))
)))

(deftest generate-triangle-test
	(testing "Should generate a template vector bearing corresponding cells' positions"
                ;;;      VECTORS OF CELLS           related to                          QUERIED CELL
		(is (= [ [[1 1]] 							;; [1 2]
			 [[1 1] [1 2]] 							;; [1 3]
			 [[1 1] [1 2] [1 3]] 					        ;; [2 1]
			 [[1 1] [1 2] [1 3] [2 1]]				        ;; [2 2]
			 [[1 1] [1 2] [1 3] [2 1] [2 2]]		                ;; [2 3]
			 [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3]]                          ;; [3 1]
			 [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1]]                    ;;[3 2]
			 [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2]]              ;;[3 3]
			]
				
				(generate-triangle-debug 3))))
)


(deftest format-triangle-test
	(testing "Should serialize a triangle into a line-formatted string"
		(is (= "[[[1 1]]\n [[1 1] [1 2]]\n [[1 1] [1 2] [1 3]]\n [[1 1] [1 2] [1 3] [2 1]]\n [[1 1] [1 2] [1 3] [2 1] [2 2]]\n [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3]]\n [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1]]\n [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2]]\n]"
			   (format-triangle (generate-triangle-debug 3))))))
			   
			   
(deftest merge-forkey-and-merge-value-test
  (testing "Should access nested maps and merge onto the first one"
    (let [ m1 {:a 1} m2 {:a 2} m3 {:a {:b {:c 10}}} m4 {:hello {:world 51}} ]
      (is (= {:a 3} (merge-forkey + m1 [:a] m2)))
      (is (= {:a 11} (merge-forkey + m1 [:a] m3 [:a :b :c])))
      (is (= {:hello {:world 61}} (merge-forkey + m4 [:hello :world] m3 [:a :b :c])))
      (is (= {:a 52} (merge-value + m1 [:a] 51))))))
				
(deftest surrounding-cells-test
  (testing "Should return a sorted set of surronding cells within the grid"
    (let [ siz 5 
           l-top [1 1] c-top [1 3] r-top [1 5]
           l-mid [3 1] c-mid [3 3] r-mid [3 5]
           l-bot [5 1] c-bot [5 3] r-bot [5 5]
           extra [4 4]
           
           l-top-exp (sorted-set [1 2] [2 2] [2 1])
           c-top-exp (sorted-set [1 2] [1 4] [2 4] [2 3] [2 2])
           r-top-exp (sorted-set [1 4] [2 5] [2 4])
           l-mid-exp (sorted-set [2 1] [2 2] [3 2] [4 2] [4 1])
           c-mid-exp (sorted-set [2 2] [2 3] [2 4] [3 4] [4 4] [4 3] [4 2] [3 2])
           r-mid-exp (sorted-set [2 4] [2 5] [4 5] [4 4] [3 4])
           l-bot-exp (sorted-set [4 1] [4 2] [5 2])
           c-bot-exp (sorted-set [4 2] [4 3] [4 4] [5 4] [5 2])
           r-bot-exp (sorted-set [4 4] [4 5] [5 4])
           extra-exp (sorted-set [3 3] [3 4] [3 5] [4 5] [5 5] [5 4] [5 3] [4 3]) 
         ]
         
         (is (= l-top-exp (surrounding-cells l-top 5)))
         (is (= c-top-exp (surrounding-cells c-top 5)))
         (is (= r-top-exp (surrounding-cells r-top 5)))
         (is (= l-mid-exp (surrounding-cells l-mid 5)))
         (is (= c-mid-exp (surrounding-cells c-mid 5)))
         (is (= r-mid-exp (surrounding-cells r-mid 5)))
         (is (= l-bot-exp (surrounding-cells l-bot 5)))
         (is (= c-bot-exp (surrounding-cells c-bot 5)))
         (is (= r-bot-exp (surrounding-cells r-bot 5)))
         (is (= extra-exp (surrounding-cells extra 5))))))
         
(deftest next-irregular-test
	(testing "Should find the next cell which is not baseline-related starting from a designated location"
		(is (= [2 3] (next-irregular [1 1] [1 1] 5)))
		(is (= [2 3] (next-irregular [1 1] [1 2] 5)))
		(is (= [2 3] (next-irregular [1 1] [1 3] 5)))
		(is (= [2 3] (next-irregular [1 1] [1 4] 5)))
		(is (= [2 3] (next-irregular [1 1] [1 5] 5)))
		(is (= [2 3] (next-irregular [1 1] [2 1] 5)))
		(is (= [2 3] (next-irregular [1 1] [2 2] 5)))		
		(is (= [2 3] (next-irregular [1 1] [2 3] 5)))
		(is (= [2 4] (next-irregular [1 1] [2 4] 5)))		
		(is (= [3 2] (next-irregular [1 1] [3 1] 5)))
		
		(is (= [4 1] (next-irregular [3 3] [3 3] 5)))
		(is (= [4 5] (next-irregular [3 3] [4 2] 5)))
		
		(is (= nil (next-irregular [5 1] [5 2] 5)))
		
))		
        
(deftest is-envelope?-test
    (testing "Should return true iff a collection is a wrapper around another"
        (let [ coll-1 [1 2 3] coll-2 [[1][2]] 
               coll-3 [[1 2 3]]
               coll-4 [[[[1 2 3]]]]
               coll-5 (list 1 2 3)
               coll-6 (list (list 1) (list 2))
               coll-7 (list (list 1 2 3))
               coll-8 (list (list (list (list 1 2 3))))
            ]
        (is (not (is-envelope? coll-1)))
        (is (not (is-envelope? coll-2)))
        (is (is-envelope? coll-3))
        (is (is-envelope? coll-4))
        (is (not (is-envelope? coll-5)))
        (is (not (is-envelope? coll-6)))
        (is (is-envelope? coll-7))
        (is (is-envelope? coll-8)))))

(deftest distribute-test
    (testing "Should distribute an element over a collection"
        (is (= (list (list 1 1) (list 1 2) (list 1 3)) (distribute 1 [[1][2][3]])))
        (is (= (list (list :a :b :c :d) (list :a :e)) (distribute :a [(list :b :c :d) (list :e)])))
        (is (= (list (list :a :b :c) (list :a :d) (list :a :e :f :g)) (distribute :a (list (list :b :c) :d (list :e :f :g)))))
))

(deftest unfold-test
	(testing "Should remove outermost nesting of sub sequences only"
		(let [
				;; (1 2 3 (4 5)) -> (1 2 3 4 5)
				one (unfold (list 1 2 3 (list 4 5)))
				exp_one (list 1 2 3 4 5)
								
				;; (1 (1) (2) (3) 4 5) -> (1 2 3 4 5)
				two (unfold (list (list 1) (list 2) (list 3) 4 5))
				exp_two (list 1 2 3 4 5)
				
				;; ((1 2) (4 (5 6))) -> (1 2 4 (5 6))
				three (unfold (list (list 1 2) (list 4 (list 5 6)))) 
				exp_three (list 1 2 4 (list 5 6))
				
				;; (((::bb :b1 :b3) (:bb :b1 :b2) (:bb :b1 :b4 :b5)) :b) -> ((::bb :b1 :b3) (:bb :b1 :b2) (:bb :b1 :b4 :b5) :b)
				four  (unfold (list (list (list :bb :b1 :b3) (list :bb :b1 :b2) (list :bb :b1 :b4 :b5)) :b))
				exp_four (list (list :bb :b1 :b3) (list :bb :b1 :b2) (list :bb :b1 :b4 :b5) :b)
			 ]
			 
			(is (= exp_one one))
			(is (= exp_two two))
			(is (= exp_three three))
			(is (= exp_four four))
)))			

 
(deftest is-envelope?-test2
	(testing "Should detect superfluous nested wrappers"
		(let [ arg1 '(1 2 3 4 (5 6))
			   exp1 false
			   
			   arg2 '((1 2 3))
			   exp2 true
			   
			   arg3 '((((((1))))))
			   exp3 true
			   
			   arg4 '(([:a :b]))
			   exp4 true
			   
			   arg5 '(([1 2]) ([3 4]))
			   exp5 false
			 ]
			 
		(is (= exp1 (is-envelope? arg1)))
		(is (= exp2 (is-envelope? arg2)))
		(is (= exp3 (is-envelope? arg3)))
		(is (= exp4 (is-envelope? arg4)))
		(is (= exp5 (is-envelope? arg5))))))


(deftest unwrap-test
	(testing "Should get rid of superfluous nested wrappers"
		(let [ arg1 '(1 2 3 4 (5 6))
			   exp1 arg1
			   
			   arg2 '((1 2 3))
			   exp2 '(1 2 3)
			   
			   arg3 '((((((1))))))
			   exp3 '(1)
			   
			   arg4 '(([:a :b]))
			   exp4 '[:a :b]
			   
			   arg5 '(([1 2]) ([3 4]))
			   exp5 arg5
			 ]
			 
		(is (= exp1 (unwrap arg1)))
		(is (= exp2 (unwrap arg2)))
		(is (= exp3 (unwrap arg3)))
		(is (= exp4 (unwrap arg4)))
		(is (= exp5 (unwrap arg5))))))

		
(defn vkpr 
"Verify key-path result. Ensures that actual and expected values are the same, not considering order.
"
	[expected actual] 
		;; Every 'actual' elem is found in 'expected' and vice-versa
		(and (every? (fn [ arg] (some #(= arg %) expected)) actual)
			(= (count actual) (count expected))))

(deftest key-paths_and_leaves-test
    (testing "Should yield all key-paths leading to values in nested maps, from arg. map
    		 Furthermore, each key-paths element must yield the correct value using clojure.core/get-in.
    		 Also testing leaves function."
    		 
        (let [ m1 {:a 
                    {:b [1 2] 
                     :bb 
                        {:b1 
                            {:b2 [7 8] 
                              :b3 [9 10] 
                              :b4
                                  {:b5 [11 12]}}}} 
                   :c {:d {:e [3 4] :f [5 6]}}} 

                m1-exp '((:a :b) (:a :bb :b1 :b2) (:a :bb :b1 :b3) (:a :bb :b1 :b4 :b5)
                		  (:c :d :e) (:c :d :f))
                   			 
                m1-leaves '([1 2] [7 8] [9 10] [11 12] [3 4] [5 6])
                   			 
				m2 {:a 1 :b 2 :c 3 :d 4}
				
				m2-exp '((:a) (:b) (:c) (:d))
				
				m2-leaves '(1 2 3 4)
				
				m3 {:a {:b {:c {:d {:e "enough"}}}}}
				m3-exp '((:a :b :c :d :e))
				m3-leaves '("enough")
				
				m4 {:a 1 
					:b { 
						:c 2 
						:d 3 
						:e 
							{:f 4 :g 5}}
					:h 6
					:i { :j 7}
					:k { :l 8 :m 9}
				   }
				   
			   m4-exp '( (:a)
			   		   (:b :c) 
			   		   (:b :d)
			   		   (:b :e :f)
			   		   (:b :e :g)
			   		   (:h)
			   		   (:i :j)
			   		   (:k :l)
			   		   (:k :m))
				                   		
			   m4-leaves '(1 2 3 4 5 6 7 8 9)
              ]
            (is (vkpr m1-exp (key-paths m1)))
            (is (vkpr m1-leaves (leaves m1)))
            (is (vkpr m2-exp (key-paths m2)))
            (is (vkpr m2-leaves (leaves m2)))            
            (is (vkpr m3-exp (key-paths m3)))
            (is (vkpr m3-leaves (leaves m3)))            
            (is (vkpr m4-exp (key-paths m4)))
            (is (vkpr m4-leaves (leaves m4)))     
            
            ;;border/degenerate cases       
            (is (= (list) (key-paths {})))            
            (is (= 3) (key-paths 3))
            (is (= nil (key-paths nil)))            
    )))
      
(deftest serialize_and_deserialize-kp-test
 	(testing "Should serialize and deserialize keypaths and from map accurately"
 		(let [
 				;;; Using m4 from key-paths_and_leaves-test above:
 				;;; as the backing map for kps biding
 				m 	{:a 1 
					:b { 
						:c 2 
						:d 3 
						:e 
							{:f 4 :g 5}}
					:h 6
					:i { :j 7}
					:k { :l 8 :m 9}
				   }
 				
 				;;also copied from previous test's 
				kps '((:a) (:b :c) (:b :d) (:b :e :f) (:b :e :g) (:h) (:i :j) (:k :l)(:k :m))
				
				ser (serialize-kp kps)				
				serm (serialize-kp m)
				exp-ser "'((:a) (:b :c) (:b :d) (:b :e :f) (:b :e :g) (:h) (:i :j) (:k :l) (:k :m))"
				
				deser (deserialize-kp ser)
				exp-deser kps
			]
			(is (= exp-ser ser))
			(is (= exp-ser serm))
			(is (= exp-deser deser)))
		
		;;; border/degenerate cases
		(is (= "'()" (serialize-kp '())))
		(is (= "'()" (serialize-kp {})))
))


(deftest kp-path-test
	(testing "Should generate a new map consisting of key-paths and associated values from an existing map"
		(let [ 
			m1 {:a 1 :b 2 :c 3}
			exp1 { "'(:a)" 1 "'(:b)" 2 "'(:c)" 3}
			
			m2 {:a { :a1 1 :a2 2} :b 3 :c { :d {:e 4 :f { :g 5}}}}
			exp2  { "'(:a :a1)" 1 "'(:a :a2)" 2 "'(:b)" 3 "'(:c :d :e)" 4 "'(:c :d :f :g)" 5}
			
			]
			
		 (is (= exp1 (kp-map m1)))
		 (is (= exp2 (kp-map m2)))
)))		

(deftest demux-test
	(testing "Should demultiplex an input into a collection from applying filter and predicates"
		(let [
				in1 1
				coll1 [2 3 4 5 6 7 8 9]
				comb1 +
				pred1 even?
				prune1 #(< % 8)
				
				exp1 '(3 5 7)
				act1 (demux in1 coll1 pred1 comb1 prune1)
				
				exp1b '(3 5 7 9)
				act1b (demux in1 coll1 pred1 comb1)
				
				
				in2 [[1 1] [2 3]]
				coll2 (for [ x (range 1 4) y (range 1 4) ] [x y])
				comb2 #(concat %1 [%2])
				pred2 (fn [c] (not (in? (map #(first %) in2) (first c))))
				prune2 #(not (in? in2 %))
				prune2b (fn [_] false)
				
				exp2 '(([1 1] [2 3] [3 1]) ([1 1] [2 3] [3 2]) ([1 1] [2 3] [3 3]))
				exp2b '()
				
				act2 (demux in2 coll2 pred2 comb2 prune2)
				act2b (demux in2 coll2 pred2 comb2 prune2b)
				
			]
			
		  	(is (= exp1 act1))
		  	(is (= exp1b act1b))
		  	(is (= exp2 act2))
		  	(is (= exp2b act2b))
		  	
)))		  	
	
