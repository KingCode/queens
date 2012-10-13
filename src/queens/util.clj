(ns queens.util)

(defn id-generator []
    (let [id (atom 0)]
        (fn [] (swap! id inc) )))

;; Returns true if coll filtered with pred
;; contains at least one element; false otherwise.
(defmacro coll-pred [coll pred]
    `(< 0 (count (filter #(~pred %) ~coll))))    

(defn append [ coll1 coll2] (vec (concat coll1 coll2)))

(defn same? [ c1 c2 ]
	(cond (empty? c1) false
	      (empty? c2) false
	 :else
	 	(let [[x1 y1] c1 [x2 y2] c2]
			(and (= x1 x2) (= y1 y2)))))
			
(defn contains-cell? [coll elem] (coll-pred coll (partial same? elem))) 

(defn same-row? [[x1 y1] [x2 y2]] (= x1 x2))
(defn same-col? [[x1 y1] [x2 y2]] (= y1 y2))
(defn same-diag? [[x1 y1] [x2 y2]] (let [xdiff (Math/abs (- x2 x1)) ydiff (Math/abs (- y2 y1))] (= xdiff ydiff)))

;; Returns true if two cells are in the same row, column or 45 degree angle diagonal; false otherwise
(defn same-baseline? [c1 c2]
        (or (same-row? c1 c2) (same-col? c1 c2) (same-diag? c1 c2)))

;; Returns true if cell lies on the same-baseline as one of the cells in coll, and false otherwise.
;; If coll is empty false is returned
(defn same-baseline-from? [coll cell]
    (cond (empty? coll) false
    	  (same-baseline? (first coll) cell) true
          (= 1 (count coll)) false
          :else
            (recur (next coll) cell)))			

;;
;; Returns all cells in coll on the same baseline as cell
;; or nil if none is found.
;;
(defn in-baseline [coll cell]
        (vec (map #(same-baseline? % cell) coll))) 
     

;; Returns argument, a workaround for #(x) when x is not a function
(defn self [x] x)

;;
;; Returns a function which invokes this function with re-bound args, or the GCD
;; (Using Euclid's alg.
;;    \gcd(a,a) = a
;;    \gcd(a,b) = \gcd(a - b,b) [b < a]
;;    \gcd(a,b) = \gcd(a, b-a) [b > a]
;;
;; Note that zero is an invalid argument and will prevent progress from being made.
;;
(defn gcd_op [a b] 	
    (cond (= a b) #(self a) ;;NOT simply a, because (gcd 1 1) will make trampoline complain.
    		            ;;(the very first invocation must return a function, at least in 1.4)
          (< b a) #(gcd_op (- a b) b)
          :else   #(gcd_op a (- b a))))

;;
;; Returns the greatest common divisor between two non-negative, non-zero integers.
;; This should not be used with zero value argument(s), or this function will never exit.
;;           
(defn gcd [a b] (trampoline (gcd_op a b)))

;;
;; Returns the smallests slope increments between any two sorted neighbouring cells of 
;; the line segment defined by the argument cells, as a vector containing the row and column differentials,
;; respectively. The argument cells must be pre-sorted in row, column order before the computation. 
;; 
;; Use smallest-increments-between c1 c2) if the line may be horizontal or vertical, or this function will 
;; never return.
;;
(defn smallest-slope-diff-presorted [ [[x1 y1] [x2 y2]] ]
	(let [ deltaX (- x2 x1) deltaY (- y2 y1)
		   gcd_argX (Math/abs deltaX) gcd_argY (Math/abs deltaY)
		   slicer (gcd gcd_argX gcd_argY) 
		   incX (/ deltaX slicer)
		   incY (/ deltaY slicer) ]
		   
		   [incX incY] ))
;;
;; Returns the smallests coordinate increments between two (sorted) neighbouring cells of a line having a 
;; non-zero, finite slope defined by the argument cells; as a vector containing the row and column differentials,
;; respectively. The cells are sorted in row, column order during the computation. 
;; 
;; (Use smallest-increments-between c1 c2) if the line may be horizontal or vertical, or work will freeze (infinite trampoline, see ).
;; memory error may occur.
;;
(defn smallest-slope-diff [c1 c2] (smallest-slope-diff-presorted (sort [c1 c2])))			   
		   
		   
;;
;; Returns the smallest coordinate increments between two (sorted) nehgbouring cells of the line defined by 
;; c1 and c2; as a vector containig the row and column differentials, resp. 
;;		   
(defn smallest-increments-between [c1 c2] 
	(let [ sorted (sort [c1 c2]) [x1 y1] (first sorted) [x2 y2] (second sorted)					
		   diffX (- x2 x1) diffY (- y2 y1)	]

		   (cond (= 0 diffX) [0 1]
		   		 (= 0 diffY) [1 0]
		   		 :else		   		 
		   			(smallest-slope-diff-presorted sorted))))
			   					   	
;;
;; Returns true if a cell is outside a square grid of size cells per side; false otherwise
;;
(defn outside? [ [x y] size ]    
      (or (< size x) (< x 1) (< size y) (< y 1)))

;;
;; Fills a line segment starting from the start cell backward until either coordinate exceeds size, 
;; with the orientation (deltas) provided.
;;      
      
;;
;;
;; Fills a line segment starting from start cell until either coordinate exceeds size, 
;; with the orientation (deltas) provided. 
;;		   			
(defn half-line [ deltas start size acc]
	(if (outside? start size)
		acc		
 		(let [ [deltaX deltaY] deltas [x y] start ]
			(recur deltas [(+ x deltaX) (+ y deltaY)] size (conj acc start)))))

;;
;; Returns a vector of cells for the half line segment from start to the cell nearest to the grid border
;;
(defn line-forward [ deltas start size ]
	(half-line deltas start size []))

;;
;; Returns the inverse of a direction, e.g (= [-2 3] (inverse [2 -3])
;; NOTE: there is probably a built-in fn/macro to do sign reversal, I just have to find it...
;;
(defn inverse [[x y]] [(- x x x) (- y y y)])

;;
;; Returns a vector of cells for the half line segment from start to the cell nearest to the grid border
;;			   					   			
(defn line-backward [ deltas start size ]
	(vec (half-line (inverse deltas) start size '())))
;;
;; Builds and returns a fully constructed line with coordinates not exceeding 1 or size sorted in row/col. order, with the 
;; orientation (deltas) and seed cell provided; if provided acc is expected to be either []
;; or a partially constructed line.
;;
(defn full-line [ deltas start size ]
        (let [ head (line-backward deltas start size)
               tail (line-forward deltas start size) ]
        (vec (concat (pop head) tail))))	

;;
;; Returns the line defined by cells c1 and c2 in a grid of side dimension size
;; as a vector of cells sorted in row/col order.
;;
(defn line [ c1 c2 size ]
    (let [ deltas (smallest-increments-between c1 c2) ]
        (full-line deltas c1 size)))

;;
;; Filters cells from coll which form a baseline with c
;;
(defn filter-baselines [ coll c] 
    (filter #(not (same-baseline? % c)) coll))

;;
;; Returns a vector of all lines formed by the cells in coll and c
;; which are irregular.
;;
(defn irregular-lines [ coll c size]
    (let [fcoll (filter-baselines coll c) ]
        (vec (map #(line % c size) fcoll)))) 

;;
;; Returns true if cell is in any one or more of lines; false otherwise.
;; Returns false if lines or cell are nil or empty 
;;
(defn any-line? [ lines cell ]
    (< 0 (count (filter #(contains-cell? % cell) lines))))

;;
;; Returns all line segments formed between any cell(s) in 'cells' forming a line 
;; segment with 'c' which is  part of any one of 'lines'; and all cells in 'cells'
;; forming a baseline with 'c'.
;; Each element in the returned vector is ordered in row/column order and can be either
;; a single cell or a pair, e.g. [s] if c defines a baseline with s or [s t] if
;; s, t and c are part of a non-baseline. Baseline partners are listed first.
;;
;; It is assumed that each one of 'lines' contains an element from 'cells'.
;;
;;(defn common-segments [ cells lines c ]
 ;;   (let [ bases (in-line cells c)     
