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

    

