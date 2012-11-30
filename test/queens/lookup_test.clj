(ns queens.lookup-test
	(:use 
			clojure.test 
			queens.test-util	
			queens.cache 
			queens.lookup))
			
			
(init-cache-and-test share-baseline-test 5
	(testing "Should tell whether two cells share a baseline"
		(is (= true (share-baseline? [1 1] [2 2])))
		(is (= true (share-baseline? [2 1] [3 2])))
		(is (= false (share-baseline? [1 2] [3 3] [2 5])))
		))			

(comment
(init-cache-and-test compose-test 5 
	(testing "Should compose a compliant candidate solution"
		(println (compose 5))
		(is (= 5 (count (compose 5 [1 1]))))
		
		))
	)	
		
