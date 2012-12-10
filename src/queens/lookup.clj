(ns queens.lookup
	(:use queens.cache queens.util))
	
(require '(clojure.math [combinatorics :as cb]))
(require '(clojure [set :as cs]))

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
Yields true if cell shares a line with any two cells in coll, and nil otherwise.
If nb is true (the default if not provided), baselines are not considered.
"
  ([acc coll cell nb]
    (if (empty? coll) false
      (let [ c1 (first coll)
           new-lid (line-id c1 cell)
           c1-lids (getLineIds c1)
           cell-lids (getLineIds cell)
           all (concat c1-lids cell-lids)
           filtered (if nb (filter #(not (isBaseline? %)) all) all)
           badones (more-than filtered 2)
        ]           
        (if (not (empty? badones)) true
            (recur (concat acc filtered) (rest coll) cell nb))))) 
  
  ([ coll cell nb ] (share-line-coll? '() coll cell nb))

  ([ coll cell ] (share-line-coll? coll cell true))) 
