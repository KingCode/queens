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

