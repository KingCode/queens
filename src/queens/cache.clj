(ns queens.cache
	(:use queens.util))

(def ^:dynamic lookup (atom {

                   :lines  	 (hash-map)
                   	          ;; a cache of pre-computed lines, as a map of line IDs to a (sorted ) vector of all grid cells
                                  ;; on the line: integer -> [[x1 y1] [x2 y2]...]
                                  ;;
                                  ;; The IDs are assumed to be unique and implement hashCode().
                                  ;;
                   :cells-to-lines (sorted-map)
                    		  ;; for fast lookup in :lines, of an existing line formation map of cells to line IDs used as
                                  ;; keys to :lines. [x y] -> [line-id-1 line-id-2 line-id-3...].
                                  ;;
                                  ;;
                                  
                   :cellpairs-to-lines []
                   			   ;; A vector of (N-1) 1 to N-1 sized vectors where N is the grid side size, and the element value
                   			   ;; refers to a line ID in :lines which contains both cells, each of which is referenced by the element's row position, 
                   			   ;; and the element's column position respectively. 
                   			   ;;
                   			   ;; For example in a grid of size 3 X 3, line IDs for each cell after
                   			   ;; cell [1 1] are stored according to the following template:
                   			   ;; 
                   			   ;; CELL : IDS OF LINES DEFINED BY PAIRING EACH OF THESE AND 'CELL'
                   			   ;;
                   			   ;; [1 2]: [1 1]
                   			   ;; [1 3]: [1 1] [1 2]
                   			   ;; [2 1]: [1 1] [1 2] [1 3]
                   			   ;; [2 2]: [1 1] [1 2] [1 3] [2 1]
                   			   ;; [2 3]: [1 1] [1 2] [1 3] [2 1] [2 2]
                   			   ;; [3 1]: [1 1] [1 2] [1 3] [2 1] [2 2] [2 3]
                               ;; [3 2]: [1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1]
                               ;; [3 3]: [1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2]
                               ;; 
                               ;; Therefore within the same example, using a current value of
                   			   ;; 		 [[nil]
                   			   ;; 		 [nil nil]
                   			   ;; 		 [nil nil nil]
                   			   ;; 		 [nil nil nil nil]
                   			   ;; 		 [85  nil nil nil nil]
                   			   ;; 		 [nil nil nil nil nil nil]
                               ;; 		 [nil nil nil nil nil nil nil]
                               ;; 		 [nil nil nil nil 176 nil nil nil]]
                               ;;
                               ;; cells [1 1] and [2 3] define the line with ID 85, and likewise for cells [3 3] and [2 2] for line with ID 176,
                               ;; and the rest are not known yet.

					:size 		0                               
					:nextId 	0  ;; sequentially unused, i.e. next available, line ID 
}))

(defn init-lookup [ size] (reset! lookup (merge @lookup {:size size :cellpairs-to-lines (generate-triangle size)})))


;;
;; Retrieves the ID of the line defined by two cells, or, assuming [x1 y1], [x2 y2] are ordered, the value stored 
;; within @lookup :cellpairs-to-lines' outer vector index: 
;;						((N squared) - (N times (N - x2)) - (y2 - 1)) - 2
;;						 x2 * N - (N - y2) - 2
;; and inner vector index:
;;						 x1		
;;
(defn get-lineID [ c1 c2 ]

	   (let [ [[x1 y1] [x2 y2]] (vec (sort [c1 c2]))
	   		  matrix (:cellpairs-to-lines @lookup)
	   		  N (:size @lookup)
	   		  row (- (- (* N x2) (- N y2)) 2) ;; decrement by 2 to account for: first grid element removed and zero-based vector
	   		  col (dec (- (* N x1) (- N y1))) ;; here we include the first element
	   		]	   		  
	   		  (nth (nth matrix row) col)))

	




