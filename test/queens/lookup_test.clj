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

(init-cache-and-test share-baseline-coll?-test 5
    (testing "Should return true at the first found baseline relation if any, false otherwise"
        (let [ coll1 '([1 1] [2 4]) 
               c1-1 [2 2]
               c1-2 [3 5]
               c1-3 [5 3]

               exp1-1 true
               act1-1 (share-baseline-coll? coll1 c1-1)

               exp1-2 true
               act1-2 (share-baseline-coll? coll1 c1-2)

               exp1-3 false
               act1-3 (share-baseline-coll? coll1 c1-3)
            ]
          (is (= exp1-1 act1-1))
          (is (= exp1-2 act1-2))
          (is (= exp1-3 act1-3))
)))

(init-cache-and-test share-line-coll?-test 5
    (testing "Should return true at the first found line relation if any, false otherwise"
        (let [ col1 '([1 1][2 3])
               c1 [3 5]
               exp1 true
               act1 (share-line-coll? col1 c1)
             ]

        (is (= exp1 act1))
)))
