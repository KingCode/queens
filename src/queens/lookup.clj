(ns queens.lookup
	(:use queens.cache queens.util))
	
(require '(clojure.math [combinatorics :as cb]))
(require '(clojure [set :as cs]))

(def LOOKUP_DEBUG false)
;;
;; Utilities similar to queens.util, except that the cache lookup is leveraged for
;; constant time performance over time.
;;


(defn share-baseline? 
"Returns true if any of the argument cells share a baseline, and false otherwise
"
  ([c1 c2]	
    (isBaseline? (line-id c1 c2)))
  ([c1 c2 & cs]		
    (let [ all (concat [c1 c2] cs),
           pairs (cb/combinations all 2) ]
	(< 0 (count (filter #(share-baseline? (first %) (second %)) pairs))))))		

(defn share-baseline-coll?
"Returns true if cell shares a baseline with any cell in coll. Yields nil otherwise.
"
    [coll cell]
        (cond (empty? coll) false
            (share-baseline? (first coll) cell) true
            :else
                (recur (rest coll) cell)))

(defn share-line?
"
Yields true if any three cells share any line. If only two arguments,
yields true if the shared line is a baseline. False otherwise.
"
    ([ c1 c2 ] (share-baseline? c1 c2))
    ([ c1 c2 c3 ]
        (let [ line-1 (line-id c1 c2) line-2 (line-id c1 c3) ]
	 			(= line-1 line-2)))
    ([ c1 c2 c3 & cs ]
	(let [ all (concat [c1 c2 c3] cs),
               triples (cb/combinations all 3) ]
	   (< 0 (count (filter #(share-line? (first %) (second %) (last %)) triples))))))

(defn share-line-coll?
"
Yields true if cell shares a line with any two cells in coll, or shares a baseline with
any one cell; false otherwise.
If nb is true (the default if not provided), baselines are not considered.
"
  ([acc coll cell nb]
    (cond (empty? coll) false
          (and (not nb) (share-baseline-coll? coll cell)) true
      :else
      (let [ c1 (first coll)
           new-lid (line-id c1 cell)
           c1-lids (getLineIds c1)
           ;;cell-lids (getLineIds cell)
           all (concat acc c1-lids (list  new-lid)) ;;cell-lids)
           filtered (if nb (filter #(not (isBaseline? %)) all) all)
           badones (more-than filtered 2)
        ]           
        (if LOOKUP_DEBUG
            (println "ACC=" acc ", COLL=" coll 
                    "\nFIRST-LID=" c1-lids "\nCELL-LID=" new-lid
                    "\nALL=" all "\nFILTERED=" filtered "\nBADONES=" badones))
        (if (not (empty? badones)) true
            (recur filtered (rest coll) cell nb))))) 
  
  ([ coll cell nb ] (share-line-coll? '() coll cell nb))

  ([ coll cell ] (share-line-coll? coll cell true))) 
  

(defn candidates-row 
"
Yields an ordered sub-sequence of all candidates on row.
If row is out of range, nil is returned.
"
  [row]
  (let [size (:size @lookup)]
  	(cond (< size row) nil
  		  (< row 1) nil
  		:else
  		(for [ y (range 1 (inc (:size @lookup))) ] [ row y]))))
	
	    
  
  
