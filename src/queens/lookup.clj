(ns queens.lookup
	(:use queens.cache queens.util))
	
(require '(clojure.math [combinatorics :as cb]))
(require '(clojure [set :as cs]))

;;
;; Utilities similar to queens.util, except that the cache lookup is leveraged for
;; constant time performance over time.
;;


(defn share-baseline? 
	"Returns true if any of the argument cells share a baseline, and false otherwise"
	([c1 c2]	
		;;(do (println "ShareBaseline c1=" c1 ", c2=" c2)
		(isBaseline? (line-id c1 c2)))
	([c1 c2 & cs]		
		(let [ all (concat [c1 c2] cs),
			   pairs (cb/combinations all 2) ]
				(< 0 (count (filter #(share-baseline? (first %) (second %)) pairs))))))		
				
(defn share-line-query? 
	"Returns true if in the argument cells, any two share a baseline or any three share a non-baseline; false otherwise"	 	 
	
	 ([c1 c2] (share-baseline? c1 c2))
	 
	 ([c1 c2 c3]
	 	(if (share-baseline? c1 c2 c3) true
	 		(let [ line-1 (line-id c1 c2) line-2 (line-id c1 c3) ]
	 			(= line-1 line-2))))
	 	
	 ([c1 c2 c3 & cs]
	 	(let [ all (concat [c1 c2 c3] cs),
	 		   triples (cb/combinations all 3) ]
	 		   (< 0 (count (filter #(share-line-query? (first %) (second %) (last %)) triples))))))
	 	 

	 		   
	 		   
(comment "	 		   
(defn free-cells 
	Composes a hierarchy of all free cells for each leaf element in the input collection, 
	 i.e. each leaf sub-sequence meets the criteria (not (share-line-query? from-input-cell cell) 
	 for the corresponding input-cell in the argument. If the input list is a hierarchy,
	 a corresponding hierarchy is generated with leaf sub-sequences for each leaf in the input.

	([ N acc all] 
		(if (zero? N) acc
			(let [ diff (cs/difference (set all) (set acc))
				   latest (filter #(not (apply share-line-query? (conj acc %))) diff) ]
				(recur (dec N) (conj acc latest) all))))
	
	([ N start ] 		
		(let [sel (cb/selections (range 1 (inc N)) 2),
			  const (vec (map vec sel)) ]
			  	(compose N [ start ] const)))
		
	([ N ] (compose N [1 1])))
	
")
	

		
	

	


	