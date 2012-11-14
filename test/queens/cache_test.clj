(ns queens.cache-test
		(:use clojure.test 
			  queens.test-util
			  queens.util
			  queens.cache
			  ))
			  

(bind-cache-and-test 
		pairs-lookup-test nil nil (generate-triangle-debug 3) 3 0
	(testing "Should retrieve the correct matrix location for the requested location"
	;;	(println "Lookup is " (:cellpairs-to-lines @lookup))
		(is (= [1 1] (get-lineID [1 1] [2 1])))
))

        
