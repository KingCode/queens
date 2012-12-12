(ns queens.core
	(:use queens.util queens.state queens.cache queens.lookup))
(require '(clojure [set :as cs]))

(comment
"
	
"
)




(defn occupied? [[x y]]  (coll-pred (:queens @state) #(same? % [x y])))

;; Wrapper around -> SEE queens.util/same-baseline-from [(:queens @state) cell
(defn same-baseline-any? [cell] (same-baseline-from? (:queens @state) cell))

(defn outside-boundary? [ cell ] (outside? cell (:size @state))) 
        
;; Calculates all cells on the same line as the two input cells
;; and returns a vector of [cell1, cell2,...] of all cells within the current size grid.
;; ASSUMPTION 1: It is assumed that this invocation is the first recognized line segment for the output line,
;;             i.e. all remaining cells on the line are deduced from the relative positions of the inputs
;; ASSUMPTION 2: The cell [x1 y1] is ordered before [x2 y2] using the natural ordering of row first, column second
(defn line-with-acc [ [x1 y1] [x2 y2] acc ]
        (let [xdiff (- x2 x1) ydiff (- y2 y1)
            newacc (conj acc [x1 y1]) ]

            (if (outside-boundary? [x2 y2])
                newacc
                (recur [x2 y2] [(+ x2 xdiff) (+ y2 ydiff)] newacc))))                       

;;
;; Returns the complete line formed by c1 and c2. This is more efficient
;; then queens.util/line which is used by ->verify, because scanning order is assumed
;; and therefore there is no need to compute increments.
;; Wrapper around -> SEE ABOVE line-with-acc
(defn line-with [ c1 c2] (line-with-acc c1 c2 []))
                       


;; Returns true if new candidate cells in 'remainder' comply with the rules, assuming 
;; cells in 'compliant' have been verified; false otherwise. If 'remainder' is nil/empty
;; true is returned.
;;
;; Both arguments are assumed to be sorted in row column order. The last argument stores non-baseline
;; formations from known cells contained in them, each with two cells from 'compliant' in it.
(defn verify ([ compliant, remainder, usedlines]
    (let [  candidate (first remainder)
            error-links (query-cells-with compliant usedlines candidate) ]
       (cond 
            (empty? remainder) true
            (not (empty? error-links))  
                            (do (assoc @state :error { candidate error-links}) false) 
          :else
            (let [ newlines (irregular-lines compliant candidate (:size @state)) ]
            (recur (conj compliant candidate) (next remainder) (append usedlines newlines))))))

;; Verifies independently (without changing the state) that all queens currently in (:queens @state) 
;; comply with the rules.
            
    ([] (verify [] (:queens @state) [])))   

          
(defn shared-baseline-filter
    [queens c] (not (share-baseline-coll? queens c)))
          
(defn overloaded-line-filter
    [queens c] (not (share-line-coll? queens c)))
    
    
(defn candidate-pred
"
Performs approval on c's compatibility with all elements of queens
"
[queens c]
	(and (shared-baseline-filter queens c) (overloaded-line-filter queens c)))

(defn inc-set
"
Emits a set of partial solutions , each starting  with queens and
one element added with its row coordinate next to the last of queens. 
If none is found an empty sequence is returned. 

If queens is already full (one for each row), it is a complete solution:
true is returned.
"
  [ queens ]	
  	(if (= (getSize) (count queens)) true
	  (let [ [lx ly] (last queens)		
                 pool (candidates-row (inc lx))
                 cpred #(candidate-pred queens %)
                 combinator #(conj-end %1 %2)
		]
     	    (demux queens pool cpred combinator))))

     	
(defn solutions 
"
Yields all possible solutions for the argument size.
"     	
  [ size ]
  	(init-lookup size)
  	(for [ y (range 1 (inc size)) ]
  		(redux inc-set (list [1 y])) ))
