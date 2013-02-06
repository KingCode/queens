(ns queens.test-util
	(:use clojure.test
			queens.state
      queens.cache     
		  queens.core
		  queens.util ))

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
	[ test-name lines cells-to-lines pairs-to-lines size nextId & body ]
	`(deftest ~test-name
		(binding [lookup (atom {
					 :lines ~lines
					 :cell2lines ~cells-to-lines
					 :matrix ~pairs-to-lines
					 :size ~size
					 :nextId ~nextId
			 }) ]
					(do ~@body))))
					
(defmacro init-cache-and-test [ test-name size & body ]  					
`(deftest ~test-name
      (init-lookup ~size)
					(do ~@body)))

										
(defmacro init-cache-lite-and-test [ test-name size & body ]  					
`(deftest ~test-name
      (init-lookup-lite ~size)
					(do ~@body)))
					

					
