(ns queens.test-util
	(:use clojure.test
		  queens.core))

;;define a test with automated global var overriding
(defmacro def-btest [test-name size queens lines & body]
    `(deftest ~test-name
        (binding [state (atom {
                                :queens ~queens
                                :size ~size
                                :lines ~lines
                          })]
                     (do ~@body))))