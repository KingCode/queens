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
                                  
                   :matrix []
                   	  ;; For fast lookup of cell pairs -> line ID for line defined by such cells.
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
                      ;;	 [[nil]
            	      ;;	 [nil nil]
                      ;;	 [nil nil nil]
                      ;; 	 [nil nil nil nil]
                      ;; 	 [85  nil nil nil nil]
                      ;; 	 [nil nil nil nil nil nil]
                      ;; 	 [nil nil nil nil nil nil nil]
                      ;; 	 [nil nil nil nil 176 nil nil nil]]
                      ;;
                      ;; cells [1 1] and [2 3] define the line with ID 85, and likewise for cells [3 3] and [2 2] for line with ID 176,
                      ;; and the rest are not known yet.

		:size 		0 ;; grid side length, must be the same as (:size @state)
		              
		:nextId 	0  ;; sequentially unused, i.e. next available, line ID 
}))

;;
;; Initialization of lookup with grid size (side length). Must be invoked consistently with (:size @state),
;;
(defn init-lookup [ size] (reset! lookup (merge @lookup {:size size :matrix (generate-triangle size)})))

;;
;; Computes row index of (:matrix @lookup) for argument cell
;;
(defn- matrix-rowFor [ [x y]]
    (let [ N (:size @lookup) ]
        ;; decrement by 2 to account for: first grid element removed and zero-based vector
        (- (- (* N x) (- N y)) 2)))

;;
;; Computes column index of (:matrix @lookup) for argument cell
;;        
(defn- matrix-colFor [ [x y]]
    (let [ N (:size @lookup) ]
        (dec (- (* N x) (- N y))))) ;; here we include the first element, as opposed to matrix-rowFor
;;
;; Retrieves the ID of the line defined by two cells, or, assuming [x1 y1], [x2 y2] are ordered, the value stored 
;; within @lookup :matrix' using [x1 y1] to index the location within the structure indexed by [x2 y2].
;;
(defn get-lineID [ c1 c2 ]
    (let [ [sc1 sc2] (vec (sort [c1 c2]))
	   matrix (:matrix @lookup)
           row (matrix-rowFor sc2)
   	   col (matrix-colFor sc1)
           rowVec (nth matrix row)
  	 ]	   		  
   	  (nth (nth matrix row) col)))

;; 
;; Creates a new matrix from current values in (:matrix @lookup) and 
;; the argument value for the location corresponding to the two argument cells
;;
(defn new-matrix [ cell-1 cell-2 value] 
    (let [ [c1 c2] (vec (sort [cell-1 cell-2]))
           row (matrix-rowFor c2)
   	   	   col (matrix-colFor c1)
           matrix (:matrix @lookup)
           rowSplits (split-at (inc row) matrix)
           oldRow (last (first rowSplits))
           rowPfx (vec (butlast (first rowSplits)))
           rowSfx (vec (last rowSplits))
           colSplits (split-at (inc col) oldRow)
           colPfx (vec (butlast (first colSplits)))
           colSfx (vec (last colSplits))
           newRow (vec (concat colPfx [value] colSfx))
         ]
           (vec (concat rowPfx [newRow] rowSfx))))

(defn insert-into-matrix! [ c1 c2 val]
    (reset! lookup (merge @lookup { :matrix (new-matrix c1 c2 val) })))
    
    
;;(defn check-and-insert-into-matrix! [ c1 c2 val]
;;	(if (= nil (get-lineID c1 c2))
;;		(let [ newline (line c1 c2 
	

