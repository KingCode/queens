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

(init-cache-and-test share-line?-test 7
    (testing "Should tell whether two cells share a baseline or any three 
              any other"
        (is (share-line? [1 1] [2 2]))
        (is (share-line? [1 1] [2 3] [3 5]))
        (is (share-line? [1 1] [2 4] [3 7]))
        (is (share-line? [1 1] [3 5] [4 7]))
        (is (share-line? [1 1] [3 2] [5 3]))
        (is (share-line? [1 4] [3 3] [5 2]))
        (is (share-line? [1 6] [4 4] [7 2]))
        (is (not (share-line? [1 5] [4 4] [7 2])))
))
