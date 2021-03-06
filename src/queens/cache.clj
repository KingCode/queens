(ns queens.cache
	(:use queens.util queens.str-util))

(def ^:dynamic lookup (atom {

                   :lines  	 (sorted-map)
                   	          ;; a cache of pre-computed lines, as a map of line IDs to a (sorted ) vector of all grid cells
                                  ;; on the line: integer -> [[x1 y1] [x2 y2]...]
                                  ;;
                                  ;; The IDs are assumed to be unique and implement hashCode().
                                  ;;
                                  
				   :baselines (sorted-set)
				   			   ;; for fast lookup of whether a line is a baseline, as a set of lineIDs
				                                     
                   :cell2lines (sorted-map)
                    		  ;; for fast lookup in :lines, of an existing line formation map of cells to line IDs used as
                                  ;; keys to :lines. [x y] -> [line-id-1 line-id-2 line-id-3...].
                                  ;;
                                  ;;
                                  
                   :matrix []
                   	  ;; For fast lookup of cell pairs -> line ID for line defined by such cells.
                      ;; A vector of (N-1) 1 to N-1 sized vectors where N is the grid side size, and the element value
                      ;; refers to a line ID in :lines which contains both cells, each of which is referenced by the element's
                      ;; row position, and the element's column position respectively. 
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
		                
		              ;;
		              ;; A map of sequences of cell IDs to lazy-seq's of grid cells susbsets used 
		              ;; as a source to pick candidates from.
		              ;;
                                :candidates {}						
								                
}))   


(defn init-lookup 
"
Initialization in current binding of lookup with grid size (side length). 
"
  ( [ size startId] 
    (let [limit (inc size)]
      (reset! lookup 
          { :lines (sorted-map)
            :cell2lines (sorted-map)            
            :matrix (generate-triangle size)
            :size size 
            :nextId startId            
            :candidates (for [ x (range 1 limit) y (range 1 limit) ]
            						[x y])
            })))
            
  ( [ size ]
    (init-lookup size 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(defn init-lookup-lite
"
Same as init-lookup except that (:matrix lookup) is not initialized to save space.
Must not be used with any line-related utility - use init-lookup otherwise.
"
  [ size ]
    (let [ limit (inc size) ]
    	(reset! lookup
		  { :size size 
            :candidates (for [ x (range 1 limit) y (range 1 limit) ]
            						[x y])
    	  })))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- matrix-rowFor [ [x y]]
"
Computes row index of (:matrix @lookup) for argument cell
"
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
(defn find-lineId 
    ([ c1 c2 m ]
        (let [  [sc1 sc2] (vec (sort [c1 c2]))
                row (matrix-rowFor sc2)
   	   	        col (matrix-colFor sc1)
                rowVec (nth m row)
  	     ]	   		  
   	     (nth (nth m row) col)))

    ([ c1 c2 ]
	(find-lineId c1 c2 (:matrix @lookup))))
   	  
  
;;;;;;;;                                                                  ;;;;;;;;;;
;;;;;;;; ACCESSOR FUNCTIONS - convenience utilities and wrappers -- which ;;;;;;;;;;
;;;;;;;; do not perform any 'corrective' action, such as create an entry  ;;;;;;;;;;
;;;;;;;; if it does not exit.                                             ;;;;;;;;;;
;;;;;;;;                                                                  ;;;;;;;;;;
;;;;;;;; Each invocation is done atomically, i.e. with a time 'snapshot'  ;;;;;;;;;;
;;;;;;;; of the data.                                                     ;;;;;;;;;;
;;;;;;;;                                                                  ;;;;;;;;;;

;;
;; Retrieves the line IDs for 'cell', or an empty vector if not found.
;;
(defn getLineIds [ cell ]
  (get (:cell2lines @lookup) cell []))

;;
;; Retrieves all lines 'cell' belongs to, 
;; as a vector of vectors of cell coordinates [x y];
;; or an empty vector if none are found.
;;
(defn getLines [ cell ]    
  (let [ lu @lookup 
         lineIds (get (:cell2lines lu) cell []) ]
    (vec (map #(get (:lines lu) %) lineIds))))
         
;;
;; Retrieves the line ID of the line defined by
;; c1 and c2. Same as (find-line c1 c2)
;;
(defn getLineId [ c1 c2 ]
  (find-lineId c1 c2))
  

;;
;; Retrieves a line (vector of cells) for either a line defined by two cells
;; or for a lineID; nill is returned if not found.
;;
(defn getLine 
  ;;
  ;; Retrieves the line defined by c1 and c2,
  ;; or nil if none exit.
  ([ c1 c2 ]
    (get (:lines @lookup) (find-lineId c1 c2) nil))
  
  ;;
  ;; Retrieves the line with id lineID, or nil if not found
  ([ lineID ]
    (get (:lines @lookup) lineID)))
    
;;
;; Returns true if a lineId exists and is for a baseline, false otherwise.
;;    
(defn isBaseline? [ lineId ] (in? (:baselines @lookup) lineId))
    
;;
;; Retrieves the size of the grid side.
;;    
(defn getSize [] (:size @lookup))    

(defn candidates [] (:candidates @lookup))
;;;;;;;;;;;;;;;;;;;;;;;;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; END, ACCESSOR FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;; 
;; Creates a new matrix from current values in (:matrix @lookup) and 
;; the argument value for the location corresponding to the two argument cells
;;
(defn new-matrix 
    ( [ cell-1 cell-2 value matrix] 
        (let [ [c1 c2] (vec (sort [cell-1 cell-2]))
                row (matrix-rowFor c2)
      	        col (matrix-colFor c1)
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
           
    ([ c1 c2 value ]
	(new-matrix c1 c2 value (:matrix @lookup))))           

;;
;; Returns a new matrix by entering 'id' for the seond of each cell pair of 'pairs'
;; in the input matrix.
;; 
(defn new-matrix-from-pairs [ id pairs matrix]
    (if (empty? pairs) matrix
        (let [ [c1 c2] (first pairs) ]
            (recur id (rest pairs) (new-matrix c1 c2 id matrix))))) 

;;
;; Generates a collection of ordered pairs between line-cells.
;;
(defn make-pairs-from-line [line]
    (reverse (sort (for [ x line y line :while (before? y x) ] [x y]))))

;;
;; Returns a new matrix by entering 'id' for all pairs of cells from an ordered line 
;; added to the argument matrix with the line id. 
;;
 (defn new-matrix-with-line [ id line matrix ]
    (if (> 2 (count line)) matrix
        (let [ pairs (make-pairs-from-line line) ]
            (new-matrix-from-pairs id pairs matrix))))
        

;;
;;
;;
(defn insert-into-matrix! [ c1 c2 val]
    (reset! lookup (merge @lookup { :matrix (new-matrix c1 c2 val) })))

;;
;; Returns an updated version of cell2lines with a new line Id for cell added
;;
(defn addline-to-cell [cell lineId clmap]
    (let [ lines (get clmap cell []) ]
        (assoc clmap cell (conj lines lineId))))

;;
;; Updates all entries in 'clmap' with 'lineId' added for each cell in 'line'
;;
(defn addline-to-cells [lineId line clmap]
    (if (empty? line) clmap
        (let [ updated (addline-to-cell (first line) lineId clmap) ]
            (recur lineId (rest line) updated))))

;;
;; Updates the lookup transactionally with a new line.
;;
(defn check-and-insert-newline-into-matrix! [ c1 c2 ]
	(swap! lookup (fn [ lu ]
		(if (= nil (find-lineId c1 c2 (:matrix lu)))
				(let [ 
              newline (line c1 c2 (:size lu)) 
              newId (:nextId lu)
              blines (:baselines lu)
			  newBlines (if (same-baseline? c1 c2) (conj blines newId) blines )
              newMatrix (new-matrix-with-line newId newline (:matrix lu))
              newLines (assoc (:lines lu) newId newline)
              newCell2lines (addline-to-cells newId newline (:cell2lines lu))
              ]
            (merge lu { :lines newLines :matrix newMatrix :nextId (inc newId)
                        :cell2lines newCell2lines :baselines newBlines}))))))

;;
;; An entry point into making an insert atomically into the lookup cache.
;; 
;; Retrieves the line ID for cells. If it is not found the line is computed
;; and its ID returned. All lookup structures are updated atomically.
;;
(defn line-id [ c1 c2 ]    
  (let [ id (find-lineId c1 c2) ]
	  (if (not (= nil id)) id	  
		    (do (check-and-insert-newline-into-matrix! c1 c2)
		        (find-lineId c1 c2)))))

;;
;; An entry point into making an insert atomically into the lookup cache.		
;;
;; Retrieves the line (a vector of cells) for the two argument cells,
;; creating it if necessary.
;;
(defn lookup-line [ c1 c2 ]
    (let [ id (line-id c1 c2) ]
        (get (:lines @lookup) id)))
  

(defn format-lu-matrix []
    (format-matrix (:matrix @lookup) (:size @lookup) "  -  " " " 5))
        
;;
;; For debugging, prints lookup state.
;;  
(defn show-lookup []
  (let [ lu @lookup ]
    (println (str "LOOKUP: (size " (:size lu) ")\n\tLines: " (:lines lu) "\n\tCell2Lines: " (:cell2lines lu) 
    				"\n\tBaselines: " (:baselines lu) "\n\tMatrix:\n"
          				(format-matrix (:matrix lu) (:size lu) "  -  " " " 5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          				
(defn cell-id 
"Deterministically computes the unique ID for cell in the current grid size.
 Although not (currently) used internally, it is more convenient to use IDs than cell
 coordinates as mapping keys.
"
	[ [x y] ]
	(let [len (:size @lookup)]
		(+ (* (dec x) len) (dec y))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cell-for
"Yields the cell coordinates for cell with 'id'. Id must be consistent with 
 cell-id func.
"
	[ id ]
	(let [len (:size @lookup)
		  rows (quot id len)
		  cols (mod id len)
		 ]
		 [ (inc rows) (inc cols) ] ))
		
	
          				
