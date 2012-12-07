(ns queens.cache-test
		(:use clojure.test 
			  queens.test-util
			  queens.util
			  queens.cache
			  ))
			  
(bind-cache-and-test pairs-lookup-test nil nil (generate-triangle-debug 5) 5 0
	(testing "Should retrieve the correct matrix location for the requested location"
	;;	(println "Lookup is " (:cellpairs-to-lines @lookup))
		(is (= [1 1] (find-lineId [1 1] [2 1])))
                (is (= [1 1] (find-lineId [2 1] [1 1])))
                (is (= [2 1] (find-lineId [3 2] [2 1])))
                (is (= [2 1] (find-lineId [2 1] [3 2])))
                (is (= [3 3] (find-lineId [4 1] [3 3])))
                (is (= [3 2] (find-lineId [3 2] [5 5])))
                (is (= [1 2] (find-lineId [5 1] [1 2])))))

(bind-cache-and-test insert-into-matrix!-test-debug nil nil (generate-triangle-debug 19) 19 0
      (testing "Should place an arbitrary value in correct location"
            (insert-into-matrix! [2 1] [1 1] "1 1") 
               ;; (println "New matrix: " (format-triangle nm "]]" "]]\n"))
               ;; (println "After update: " (format-triangle (:cellpairs-to-lines @lookup)))
                (is (= "1 1" (find-lineId [1 1] [2 1])))
            (insert-into-matrix! [3 2] [1 3] "1 3")
            (insert-into-matrix! [1 15] [2 3] "2 3")
            (insert-into-matrix! [2 17] [15 3] "2 17")
            (insert-into-matrix! [10 12] [6 7] "6 7")
            (insert-into-matrix! [13 13] [7 8] "7 8")
                (is (= "1 3" (find-lineId [1 3] [3 2])))
                (is (= "2 3" (find-lineId [2 3] [1 15])))
                (is (= "2 17" (find-lineId [15 3] [2 17])))
                (is (= "6 7" (find-lineId [6 7] [10 12])))
                (is (= "7 8" (find-lineId [7 8] [13 13])))
            (insert-into-matrix! [5 6] [9 1] 88)
            (insert-into-matrix! [5 6] [1 1] 174)
            (insert-into-matrix! [19 13] [2 10] 200)
                (is (= 88 (find-lineId [9 1] [5 6])))
                (is (= 174 (find-lineId [1 1] [5 6])))
                (is (= 200 (find-lineId [19 13] [2 10])))))

(bind-cache-and-test insert-into-matrix!-test nil nil (generate-triangle 19) 19 0
      (testing "Should place an arbitrary value in correct location and check that untouched locations are empty"
            (insert-into-matrix! [2 1] [1 1] "1 1") 
               ;; (println "New matrix: " (format-triangle nm "]]" "]]\n"))
               ;; (println "After update: " (format-triangle (:cellpairs-to-lines @lookup)))
                (is (= "1 1" (find-lineId [1 1] [2 1])))
            (insert-into-matrix! [3 2] [1 3] "1 3")
            (insert-into-matrix! [1 15] [2 3] "2 3")
            (insert-into-matrix! [2 17] [15 3] "2 17")
            (insert-into-matrix! [10 12] [6 7] "6 7")
                (is (= nil (find-lineId [6 7] [7 8])))
                (is (= nil (find-lineId [2 2] [3 3])))
            (insert-into-matrix! [13 13] [7 8] "7 8")
                (is (= "1 3" (find-lineId [1 3] [3 2])))
                (is (= "2 3" (find-lineId [2 3] [1 15])))
                (is (= "2 17" (find-lineId [15 3] [2 17])))
                (is (= "6 7" (find-lineId [6 7] [10 12])))
                (is (= "7 8" (find-lineId [7 8] [13 13])))
            (insert-into-matrix! [5 6] [9 1] 88)
                (is (= nil (find-lineId [6 7] [7 8])))
                (is (= nil (find-lineId [2 2] [3 3])))            
            (insert-into-matrix! [5 6] [1 1] 174)
            (insert-into-matrix! [19 13] [2 10] 200)
                (is (= 88 (find-lineId [9 1] [5 6])))
                (is (= 174 (find-lineId [1 1] [5 6])))
                (is (= nil (find-lineId [6 7] [7 8])))
                (is (= nil (find-lineId [2 2] [3 3])))                
                (is (= 200 (find-lineId [19 13] [2 10])))))

                
(init-cache-and-test check-and-insert-newline-into-matrix!-test 5
  (testing "Should make inserts consistently and atomically with only NEW cells 1,1 and 2,5"
    (let [ c1 [1 1] c2 [2 5] 
           expected-line [[1 1][2 5]]
           expected-id 0
           lu (check-and-insert-newline-into-matrix! c1 c2) ]
           
         (is (= 1 (count (:lines lu))))
         (is (= 1 (:nextId lu)))
         (is (= expected-line (get (:lines lu) expected-id)))
         (is (= [expected-id] (get (:cell2lines lu) c1)))
         (is (= [expected-id] (get (:cell2lines lu) c2)))

         ;; Accessor functions
         ;; check global scope i.e. @lookup instead of lu          
         (is (= expected-id (getLineId c1 c2)))
         (is (= [expected-line] (getLines c1)))
         (is (= [expected-line] (getLines c2)))         
         (is (= expected-id (getLineId c1 c2)))
         (is (= [expected-id] (getLineIds c1)))
         (is (= [expected-id] (getLineIds c2)))         
         (is (= expected-line (getLine c1 c2)))
         (is (= expected-line (getLine expected-id)))
         (is (= 5 (getSize)))))
         
  (testing "Should make inserts consistently and atomically with cells (1 1) (2 5) and NEW cell (3 4) paired with (2 5)"
    (let [ c1 [2 5] c2 [3 4] ;;ADDED LINE
           expected-line [[2 5][3 4][4 3][5 2]]
           expected-lines-c1 [[[1 1][2 5]] expected-line]
           expected-lines-c2 [expected-line]
           expected-id 1
           expected-ids-c1 [0 expected-id]
           lu (check-and-insert-newline-into-matrix! c1 c2) ]
           
         (is (= 2 (count (:lines lu))))
         (is (= 2 (:nextId lu)))
         (is (= expected-line (get (:lines lu) expected-id)))
         (is (= expected-ids-c1 (get (:cell2lines lu) c1)))
         (is (= [expected-id] (get (:cell2lines lu) c2)))

         ;; Accessor functions
         ;; check global scope i.e. @lookup instead of lu          
         (is (= expected-id (getLineId c1 c2)))
         (is (= expected-lines-c1 (getLines c1)))
         (is (= expected-lines-c2 (getLines c2)))         
         (is (= expected-id (getLineId c1 c2)))
         (is (= expected-ids-c1 (getLineIds c1)))
         (is (= [expected-id] (getLineIds c2)))         
         (is (= expected-line (getLine c1 c2)))
         (is (= expected-line (getLine expected-id)))

         (is (= expected-id (getLineId [5 2][4 3])))         
         (is (= expected-id (getLineId [5 2][3 4])))         
         (is (= expected-id (getLineId [5 2][2 5])))         
         (is (= expected-id (getLineId [4 3][3 4])))         
         (is (= expected-id (getLineId [4 3][2 5])))         
         (is (= expected-id (getLineId [3 4][2 5])))         
    
         (is (= 5 (getSize)))))
         
  (testing "Should make inserts consistently and atomically with cells (1 1) (2 5) and NEW line from (3 4) paired with (1 1)"
    (let [ c1 [3 4] c2 [1 1] ;;ADDED LINE
           expected-line [[1 1][3 4]]
           expected-lines-c1 [[[2 5][3 4][4 3][5 2]] expected-line]
           expected-lines-c2 [[[1 1][2 5]] expected-line]
           expected-id 2
           expected-ids-c1 [1 expected-id]
           expected-ids-c2 [0 expected-id]
           lu (check-and-insert-newline-into-matrix! c1 c2) ]
           
         (is (= 3 (count (:lines lu))))
         (is (= 3 (:nextId lu)))
         (is (= expected-line (get (:lines lu) expected-id)))
         (is (= expected-ids-c1 (get (:cell2lines lu) c1)))
         (is (= expected-ids-c2 (get (:cell2lines lu) c2)))

         ;; Accessor functions
         ;; check global scope i.e. @lookup instead of lu          
         (is (= expected-id (getLineId c1 c2)))
         (is (= expected-lines-c1 (getLines c1)))
         (is (= expected-lines-c2 (getLines c2)))         
         (is (= expected-id (getLineId c1 c2)))
         (is (= expected-ids-c1 (getLineIds c1)))
         (is (= expected-ids-c2 (getLineIds c2)))         
         (is (= expected-line (getLine c1 c2)))
         (is (= expected-line (getLine expected-id)))         
         (is (= 5 (getSize))))))
                                

(init-cache-and-test get-line-test 5
  (testing "Should retrieve line IDs and update cache consistently"
    (let [ c1 [1 1] c2 [2 5] line (lookup-line c1 c2) 
           ;; this is last for change visibility
           lu @lookup 
         ]
        (is (= 1 (:nextId lu)))
        (is (= 0 (getLineId c1 c2)))
        (is (= [[1 1] [2 5]] line))
        (is (= line (getLine 0)))
        (is (= line (getLine c1 c2))))

    (let [ c1 [2 1] c2 [3 2] line (lookup-line c1 c2) lu @lookup ]
        (is (= 2 (:nextId lu)))
        (is (= 1 (getLineId c1 c2)))
        (is (= [[2 1][3 2][4 3][5 4]] line))
        (is (= line (getLine 1)))
        (is (= line (getLine c1 c2))))))
        
        
        
(init-cache-and-test isBaseline?-test 5
   (testing "Should find an existing baseline"
   		(let [	
   				c1 [1 1] 
   				c2 [1 2]
   				lineId (line-id [1 1] [2 2]) ]
   				
   			(is (true? (isBaseline? lineId))))))
   			
   			
(init-cache-and-test cell-id_and_cell-for-test 5
	(testing "Should deterministically calculate a unique ID for a cell 
			  and vice versa."
		(let [
				c1 [1 1]
				exp1 0
				ci1 (cell-id c1)
				cf1 (cell-for exp1)
				
				c2 [1 2]
				exp2 1
				ci2 (cell-id c2)
				cf2 (cell-for exp2)
				
				c3 [1 4]
				exp3 3
				ci3 (cell-id c3)
				cf3 (cell-for exp3)
				
				c4 [1 5]
				exp4 4
				ci4 (cell-id c4)
				cf4 (cell-for exp4)
				
				c5 [2 1]
				exp5 5
				ci5 (cell-id c5)
				cf5 (cell-for exp5)
				
				c6 [2 3]
				exp6 7
				ci6 (cell-id c6)
				cf6 (cell-for exp6)
				
				c7 [2 4]
				exp7 8
				ci7 (cell-id c7)
				cf7 (cell-for exp7)
				
				c8 [2 5]
				exp8 9
				ci8 (cell-id c8)
				cf8 (cell-for exp8)
				
				c9 [3 1]
				exp9 10
				ci9 (cell-id c9)
				cf9 (cell-for exp9)
				
				c10 [3 2]
				exp10 11
				ci10 (cell-id c10)
				cf10 (cell-for exp10)
				
				c11 [3 4]
				exp11 13
				ci11 (cell-id c11)
				cf11 (cell-for exp11)
				
				c12 [3 5]
				exp12 14
				ci12 (cell-id c12)
				cf12 (cell-for exp12)
				
				c13 [4 1]
				exp13 15
				ci13 (cell-id c13)
				cf13 (cell-for exp13)
				
				c14 [4 3]
				exp14 17
				ci14 (cell-id c14)
				cf14 (cell-for exp14)
				
				c15 [4 5]
				exp15 19
				ci15 (cell-id c15)
				cf15 (cell-for exp15)
				
				c16 [5 1]
				exp16 20
				ci16 (cell-id c16)
				cf16 (cell-for exp16)
				
				c17 [5 2]
				exp17 21
				ci17 (cell-id c17)
				cf17 (cell-for exp17)
				
				c18 [5 3]
				exp18 22
				ci18 (cell-id c18)
				cf18 (cell-for exp18)
				
				c19 [5 4]
				exp19 23
				ci19 (cell-id c19)
				cf19 (cell-for exp19)
				
				c20 [5 5]
				exp20 24
				ci20 (cell-id c20)
				cf20 (cell-for exp20)
		]
    
	(is (= exp1 ci1))
	(is (= c1 cf1))

	(is (= exp2 ci2))
	(is (= c2 cf2))

	(is (= exp3 ci3))
	(is (= c3 cf3))
	
	(is (= exp4 ci4))
	(is (= c4 cf4))

	(is (= exp5 ci5))
	(is (= c5 cf5))

	(is (= exp6 ci6))
	(is (= c6 cf6))
	
	(is (= exp7 ci7))
	(is (= c7 cf7))

	(is (= exp8 ci8))
	(is (= c8 cf8))

	(is (= exp9 ci9))
	(is (= c9 cf9))
		
	(is (= exp10 ci10))
	(is (= c10 cf10))

	(is (= exp11 ci11))
	(is (= c11 cf11))

	(is (= exp12 ci12))
	(is (= c12 cf12))
	
	(is (= exp13 ci13))
	(is (= c13 cf13))

	(is (= exp14 ci14))
	(is (= c14 cf14))

	(is (= exp15 ci15))
	(is (= c15 cf15))
	
	(is (= exp16 ci16))
	(is (= c16 cf16))

	(is (= exp17 ci17))
	(is (= c17 cf17))

	(is (= exp18 ci18))
	(is (= c18 cf18))
	
	(is (= exp19 ci19))
	(is (= c19 cf19))

	(is (= exp20 ci20))
	(is (= c20 cf20))

)))	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	