(ns queens.test-util
	(:use clojure.test
                  queens.state
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
