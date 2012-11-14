(ns queens.test-util
	(:use clojure.test
			queens.state
            queens.cache     
		  queens.core))

;;define a test with automated global var overriding
(defmacro def-btest 
	( [	test-name size queens lines & body	]
    `(deftest ~test-name
        (binding [state (atom {
                                :queens ~queens
                                :size ~size
                                :lines ~lines
                          })]
                     (do ~@body)))))

(defmacro def-btest-2
	( [test-name size queens lines hotcells hotlines & body ]
    `(deftest ~test-name
        (binding [state (atom {
                                :queens ~queens
                                :size ~size
                                :lines ~lines
                                :hotcells ~hotcells
                                :hotlines ~hotlines
                          })]
                     (do ~@body)))))

(defmacro bind-cache-and-test
	( [ test-name lines cells-to-lines pairs-to-lines size nextId & body ]
	`(deftest ~test-name
		(binding [lookup (atom {
					 :lines ~lines
					 :cells-to-lines ~cells-to-lines
					 :cellpairs-to-lines ~pairs-to-lines
					 :size ~size
					 :nextId ~nextId
			 }) ]
					(do ~@body)))))
