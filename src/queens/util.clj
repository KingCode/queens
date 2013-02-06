(ns queens.util)

(def UTIL_DEBUG true)

(defn id-generator []
    (let [id (atom 0)]
        (fn [] (swap! id inc) )))

;; Returns true if coll filtered with pred
;; contains at least one element; false otherwise.
(defmacro coll-pred [coll pred]
    `(< 0 (count (filter #(~pred %) ~coll))))    

(defn append [ coll1 coll2] (vec (concat coll1 coll2)))

(defn conj-end 
"Yields a sequence where elem is appended to coll as its last element.
"
  [ coll elem ]
    (let [v (vec coll)]
        (seq (conj v elem))))

(defn same? [ c1 c2 ]
	(cond (empty? c1) false
	      (empty? c2) false
	 :else
	 	(let [[x1 y1] c1 [x2 y2] c2]
			(and (= x1 x2) (= y1 y2)))))
			
(defn in? [ coll c ] (not (= nil (some #{c} coll))))

(defn zero+? [ x ] (< -1 x))

(defn not-in? [ coll c] (not (in? coll c)))
						
;;(defn contains-cell? [coll elem] (coll-pred coll (partial same? elem))) 
(defn contains-cell? [coll elem] (in? coll elem)) 


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
;; Returns all cells in coll on the same baseline as cell.
;;
(defn in-baseline [coll cell]
        (vec (filter #(same-baseline? % cell) coll))) 
     

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
(defn fast-gcd 
"Yields the gcd using clojure's native integer division"
[a b]
    (let [ sorted (sort [a b])
           small (first sorted)
           big (second sorted)
           q (/ big small)
           qs (str q)
         ]
     (if (> 0 (.indexOf qs "/")) small
        (let [
           q-parts (.split qs "/")
           big2 (Integer/valueOf (first q-parts))
           small2 (Integer/valueOf (second q-parts))
         ]
        (/ small small2) ))))

(defn faster-gcd
"
Yields the gcd using clojure.lang.ratio, numerator, denominator fields
"
[a b]
    (let [ sorted (sort [a b])
           small (first sorted)
           big (second sorted)
           q (/ big small)
        ]
      (if (not (instance? clojure.lang.Ratio q)) small
        (let [ n (.numerator q)
               d (.denominator q)
            ]
          (/ small d)))))
;;
;; Returns the greatest common divisor between two non-negative, non-zero integers.
;; This should not be used with zero value argument(s), or this function will never exit.
;;           
(defn gcd [a b]  ;; (trampoline (gcd_op a b)))
    (faster-gcd a b))
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
;; ASSUMPTION: c1 and c2 are not the same cell.
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
;; Returns true if a cell within a square grid of size cells per side; false otherwise
;;      
(defn inside? [ [x y] size ]
  (not (outside? [x y] size)))
      
;;
;; Returns a vector of cells surrounding [x y] in a grid of side length size.
;; The returned cells are sorted and within grid boundaries, i.e. between
;; 3 and 8 cells are in the result depending on location.
;;
(defn surrounding-cells [ [x y] size ]
  ;;numbered 1 to 8 clockwise starting on the upper left   
  (let [ locations [[(dec x) (dec y)]
                   [(dec x)   y    ] 
                   [(dec x) (inc y)]
                   [  x     (inc y)]
                   [(inc x) (inc y)]
                   [(inc x)   y    ]
                   [(inc x) (dec y)]
                   [  x     (dec y)]] 
       ]
       (apply sorted-set (filter #(inside? % size) locations))))     
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

(defn nil-if-empty [ x ] (if (empty? x) nil x))

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
(defn query-cells-with [ cells lines c ]
     (if (or (empty? cells) (empty? c)) []
        (let [ basecells (sort (in-baseline cells c))
            lines-with-c (filter #(contains-cell? % c) lines)
            find-cells-in-line (fn [ line ] (vec (filter #(contains-cell? cells %) line)))
            segments (sort (vec (map find-cells-in-line lines-with-c)))  
            result  (concat basecells segments)  ] 

           (vec (filter #(not (empty? %)) result)))))
           
           
;;
;; Returns the next position following the argument location, 
;; adjusting for grid size limit
;;           
(defn move-1 [ [x y ] limit ] 
	(cond (and (<= limit x) (<= limit y)) nil 
		  (<= limit y) [(inc x) 1]
		  :else [ x (inc y) ] ))


;;
;; The operation inovked by next-irregular, see below.
;;		  		  
(defn- next-irregular-op [ base start size]
	(let [
			[bx by] base [sx sy] start
			diffx (- sx bx)
			diffy (- sy by)
		 ]
		 
		 (cond 
		 	(= nil start) #(self nil)
		 	(not (same-baseline? base start)) #(self start)
		 	(= 0 diffx) (if (= size sx) #(self nil) 
		 					#(next-irregular-op base [(inc sx) 1] size))
		 	:else 
		 			#(next-irregular-op base (move-1 start size) size))))
		
;;
;; Finds the next cell not baseline-related to with, greater or equal to from.
;; If no such cell can be found, nil is returned.
;;
;; It is assumed that base, start are sorted according to parameter order.
;;
(defn next-irregular [with from size] (trampoline next-irregular-op with from size))
		  
;;
;; Compares two cells in row/col order using the java comparator semantics
;; No nils are allowed. Deals with empty/nil cells, which compare [] doesn't do.
;;
(defn compare-cells [ c1 c2 ]
	(cond (empty? c1) 1
		  (empty? c2) -1
		  :else
 			(compare c1 c2)))

;;
;; Returns true if c1 is ordered before c2, false otherwise 
;;
(defn before? [ c1 c2 ] (< (compare-cells c1 c2) 0))

;;
;; Returns true if c1 is ordered after c2, false otherwise 
;;
(defn after? [ c1 c2 ] (< 0 (compare-cells c1 c2)))		  
		  

;;
;; Implements decisions for (index coll c) (SEE fn search below), 
;; returning anonymous functions which are either a recurrence step or a truth value. 
;;
(defn search-op [ coll c padding]
     (let [ mid (quot (count coll)  2)
            splits (split-at mid coll)
            left (nth splits 0)
            right (nth splits 1)
            l-joint (last left)
            r-joint (first right)
            lpos (+ padding (dec mid))
            rpos (+ padding mid)
            rightPadding rpos
          ]
       (cond 
        	  (empty? coll) #(self -1)
        	  (nil? c) #(self -1)        	  
        	  (= c l-joint) #(self lpos)
              (= c r-joint) #(self rpos)
              (before? c l-joint) #(search-op left c padding)
              :else         #(search-op right c rightPadding))))

;;             
;; Using binary search, returns true if the cell is present in coll, false otherwise.
;; If either or both are nil false is returned.
;; ASSUMPTION: coll is sorted and without duplicates
;;
;; Trampoline wrapper around search-op.
;;                            
(defn search [ coll c] (trampoline (search-op coll c 0)))


(defn found? [ coll c] (< -1 (search coll c)))

;; Creates a vector of (N-1) 1 to N-1 sized vectors, each element having 'init-val' value if provided,
;; or a vector of the element's position within the outermost vector.
;; The inner vectors are ordered by increasing size.
(defn generate-triangle 
([N init-val]
    (let [ numRows (* N N)
           rows (range 1 (inc numRows))
           cols (range 1 (inc N)) 
           grid (for [x cols y cols]
                    (if (= ::COORDINATES init-val) [x y] init-val)) ]
	(vec (take (dec numRows) (for [i rows] (vec (take i grid)))))))
([ N ]
	(generate-triangle N nil)))
	
(defn generate-triangle-debug [ N ] (generate-triangle N ::COORDINATES))	
			
;;
;; Formats a value produced by generate-triangle into a string with inner vectors
;; separated by a newline character.
;;	
(defn format-triangle
    ( [triangle replaced replacement]
	(let [ts (str triangle)]	
		(clojure.string/replace ts replaced replacement)))
    ( [triangle]
        (format-triangle triangle "]]" "]]\n")))			
		
;;
;; Generates row labels for a matrix triangle as a sequence of cell coordinates
;;
(defn triangle-rowLabels [size]
    (let [ s (inc size) ]
   (drop 1 (for [x (range 1 s) y (range 1 s)] [x y]))))
        
;;
;; Generates column labels for a matrix triangle as a sequence of cell coordinates
;;
(defn triangle-colLabels [size]
    (let [ s (inc size) all (for [x (range 1 s) y (range 1 s)] [x y]) ]
        (take (dec (count all)) all)))
        
        
(defn merge-forkey 
 "
 Merges a single entry for key from one map into another. The resulting map will
 yield the value for key from invoking f with values for keys1 and keys2 from m1 and m2 resp.
 All other map entries in the result are those from m1.

 If m1 or m2 are nil, or keys1/2 can't be found in either m1 or m2 at the described
 location then m1 is returned.
 "
  ([ f m1 [key1 & morekeys1 ] m2 [ key2 & morekeys2 ] ]
    (let [ keys1 (cons key1 morekeys1) keys2 (cons key2 morekeys2) ]
	  (cond 
	  	(or (nil? m1) (nil? m2)) m1		
	  	(nil? (get-in m1 keys1)) m1
	  	(nil? (get-in m2 keys2)) m1
	  	:else
	  		(assoc-in m1 keys1 (f (get-in m1 keys1) (get-in m2 keys2)))))) 
	  		
	([ f m1 [key & morekeys] m2 ]
	  (let [keys (cons key morekeys)]
	    (merge-forkey f m1 keys m2 keys))))
	    
(defn merge-value 
"
Yields a new map from merging m1 with value for key(s) from the return of 
(f (get-in m keys) value).
"
	[f m1 [key & morekeys] value]
        (merge-forkey f m1 (cons key morekeys) {:dummy-key value} [:dummy-key])) 	  
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
(defn make-seq
	[ e ]    
"
Yields a sequable of x. If not a collection, x is wrapped in a single-element list.
"
    (if (coll? e) (seq e) (list e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-envelope?
"
Yields true if coll has a single element which is also a collection
"
    [coll] (cond (empty? coll) false
           :else
              (if (and (= 1 (count coll)) (coll? (first coll))) true false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unwrap
"Recursively removes empty wrappers and yields the innermost non-empty single element collection.
"
    [coll]
        (if (is-envelope? coll) (recur (first coll)) coll))                
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unfold
"Extracts the outermost nested collections into the top-level containing collection.
 Example:
 	(unfold (((:bb :b1 :b3) (:bb :b1 :b2) (:bb :b1 :b4 :b5)) :b)) yields
 		((:bb :b1 :B3) (:bb b1 :b2) (:bb :b1 :b4 :b5) :b)
"
	([acc coll]
		;;;;(do (println "UNFOLD acc=" acc ", coll=" coll)
		(let [c (make-seq coll) f (make-seq (first c))]
			(if (empty? c) acc
				(let [ newacc (concat acc f) ]
					(recur newacc (rest c))))))
	( [coll]
		(unfold (list) coll)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn distribute
"
Distributes element over each collection arg. Yields a list of multiple sequences consisting 
of prepending e to each argument. Each collection is transformed into a seqable. 
If an element is not a collection it is wrapped inside a seqable.

Examples:
	(distribute 1 [1 2 3]) yields ((1 1) (1 2) (1 3))        
	(distribute :a ((:b :c) :d (:e :f :g)) yields ((:a :b :c) (:a :d) (:a :e :f :g))
"
  ( [ e coll] 
	(unwrap (map #(cons e (make-seq %)) (make-seq coll))))

 ( [ e coll & others ]
 	(let [ head (cons e (make-seq coll))
           more (distribute e others) 
          ]
          (concat (list head) more))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn de-nest
"
Yields a sequence containing coll elements with elements of nested lists of lists extracted
into the top-level, e.g. ((:a) ((:b :c) (:c :d)) (:e :f)) yields ((:a) (:b :c) (:c :d) (:e :f))
"
	([acc coll]	
		(if (empty? coll) acc
			(let [ i (first coll) 
			   	   newacc 
			   	   	(if (and (coll? i) (< 1 (count i)) (coll? (first i))) 
			   	   					(concat acc i) 
			   	   					(concat acc (list i)))
			 	]			 
			 (recur newacc (rest coll)))))
			 
    ( [coll] (de-nest (list) coll)))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn key-paths
"
Yields a sequence of key paths leading to leaf values in a map of maps.
Mapping keys within the argument structure must not be collections or sequences,
except for strings.

Example 1: (key-paths {:a {:b [1 2]},
				 :c {:d 
				  		{:e [3 4],
				  	   	 :f [5 6] }}}}) 
	yields a collection with the same contents as ( (:a :b) (:c :d :e) (:c :d :f))

Example 2: (key-paths {:a 
                    	 {:b [1 2] 
                    	  :bb 
                    	     {:b1 
                    	         { :b2 [7 8] 
                    	           :b3 [9 10] 
                    	           :b4
                    	               {:b5 [11 12]}}}} 
                   	   :c {:d {:e [3 4] :f [5 6]}}} )
                   
	yields a collection with the same contents as  
	( (:a :b) (:a :bb :b1 :b2) (:a ;bb :b1 :b3) (:a :bb :b1 :b4 :b5)
		(:c :d :e) (:c :d :f) )
"
 [ m ]	
   (if (not (map? m)) m
	(let [ 
		ks (sort (keys m)) 
		func (fn [ k ] 
				(let [
					v (get m k)
		        	kpv (key-paths v)
		        ]
		          (if (map? v) (distribute k kpv)
			      	(make-seq k))))
			                  
	 ]
	 (de-nest (map func ks)))))		
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn leaves
"
Yields a sequence of all leaf values of m, i.e. all non-map values of m, 
including those within nested associative structures.
"	 		
  [ m ]
  	(let [ kp (key-paths m) ]
  		(map #(get-in m %) kp)))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn serialize-kp 
"
Yields a string representing of a seq. of key paths and extracts the 
key paths if the argument is a map.
"  		
	[ kp-or-map ]	
	(let [ kpom kp-or-map
		   arg (if (map? kpom) (key-paths kpom) kpom)
		   as-list (apply list (vec arg))
		  ]
		  
		  ;;This because <empty list>.toString() yields 
		  ;;an objec reference address, 
		  ;;as does java.lang.Object.toString() ?!
		  ;;Maybe due to being a constant. 
		  
		  (if (empty? as-list) 	"'()" 
		  			(str "'" as-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deserialize-kp
"
Deserializes argument previously serialized using serialize-kp.
Yields a list, elements of which are usable with clojure.core/get-in.
"
	[ serialized ]
		(load-string serialized))
  		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kp-map
"
Yields a map containing key-paths of m as keys and (leaves m) as values.
Keys are serialized into strings and can be deserialized using de/serialize-kp
"
  [ m ]
	(let [ kp (key-paths m)		   
	       kpv (map #(get-in m %) kp)
	       raw (interleave kp kpv)
	       pairs (partition 2 raw)
	       kvs-pairs (map #(let [key (first %) val (second %)] (list (serialize-kp key) val)) pairs)
	       kvs (unfold kvs-pairs)
	       ]
	      (apply sorted-map kvs)))
	      
	      
(defn counts
"
Yields a map of distinct elements in coll to the number of times each one appears.
"
  ([acc coll]
    (if (empty? coll) acc
      (let [ k (first coll)
           v (get acc k)
           numfs (if (nil? v) 0 v)
           newacc (assoc acc k (inc numfs))
        ]
        (recur newacc (rest coll)))))

  ([coll] (counts (sorted-map) coll)))

(defn more-than
"
Yields a lazy sequence of elements of coll which appear more than limit times.
"
  [coll limit]
    (let [ m (counts coll)
           kvs (seq m) 
           less (filter #(let [num (second %)] (< limit num)) kvs)
        ]
        (map #(first %) less)))        

(defn demux 
"
Constructs outputs combining the input and elements of coll for which 
(pred e) returns true. If provided and for better performance, pruner can 
pre-filter elements before processing by pred. 
Yields a sequence of the result of invoking (comb in e).
"	 		
([ in coll pred comb pruner ]	
	(for [ e coll :when (and (pruner e) (pred e)) ]
		(comb in e)))
					
([ in coll pred comb ]					
	(demux in coll pred comb (fn[_](self true)))))	 

(defn- redux-op
"
Invokes f with the first element of acc unless acc is empty, in which case result is returned.
If max is > 0 and is the same as or less than (count results), results is returned.
Otherwise recursion occurs according to the following scenarios in order.

- If f returns true then the element is removed from acc and conj'ed to result.  
- If f yields a value for which (empty? value) is true, the element is discarded.
- Else f's output replaces the element, i.e. invocations occur in depth-first traversal.   

f must have arity compatible with acc elements, and must make progress over time, 
i.e. return true or false/nil eventually.
"
  [ f acc results max]
    (when UTIL_DEBUG 
      (println "redux-op: ACC=" acc ", MAX=" max ", NUM-RESULTS=" (count results)))
	(cond (empty? acc) results
		  (and (< 0 max) (<= max (count results))) results
      :else
	    (let [ arg (first acc) 
	  	      out (f arg) 
	  	      racc (rest acc)
	  	      newacc (cond (true? out) racc
	  		 	   (empty? out) racc	  		 		
	  		 	   :else (concat out racc))
	          newresults (if (true? out) (conj results arg) results)
	  		]
	  		(when UTIL_DEBUG
	  			(println "redux-op: NEWACC=" newacc ", NUM-NEWRES=" (count newresults)))
		    (recur f newacc newresults max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		    
(defn redux
"
Invokes a 1-arity function on init-arg. f is invoked repeatedly with its own outputs 
as arguments to subsequent calls, until all have been processed. 
When f yields true its input is collected and returned from this function.

If results-coll is provided more control is possible over the collection because
successive results are conj'ed onto it.
"
	([ f init-arg ]
		(redux f init-arg [] 0))
		
	([ f init-arg upto ]
		(redux f init-arg [] upto))		
		
	([ f init-arg results-coll upto ]
		(redux-op f (list init-arg) results-coll upto)))
