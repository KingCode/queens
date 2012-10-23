(ns queens.state-test 
		(:use clojure.test
			  queens.test-util
			  queens.state))
			  
;;[test-name size 				queens lines hotcells hotlines & body ]
			  
(def-btest-2 add-hotcells-test 13 [] (hash-map) (sorted-set) (sorted-set []) 
	(testing "Should update hotcells with added cells in order"
		(add-hotcells! [[1 1]])
		(is (= #{[1 1]} (hotcells)))
		
		(add-hotcells! [[2 3] [3 6]])
		(is (= #{[1 1][2 3][3 6]} (hotcells)))
		
		(add-hotcells! [ [5 7] [4 10] [1 1]])
		(is (= #{[1 1][2 3][3 6][4 10][5 7]} (hotcells)))
))
		
		