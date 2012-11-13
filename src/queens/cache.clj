(ns queens.cache)

(def ^:dynamic state (atom {

                   :lines  	 (hash-map)
                   	          ;; a cache of pre-computed lines, as a map of line IDs to a (sorted ) vector of all grid cells
                                  ;; on the line: integer -> [[x1 y1] [x2 y2]...]
                                  ;;
                                  ;; The IDs are assumed to be unique and implement hashCode().
                                  ;;
                   :lines-lookup (sorted-map)
                    		  ;; for fast lookup in :lines, of an existing line formation map of cells to line IDs used as
                                  ;; keys to :lines. [x y] -> [line-id-1 line-id-2 line-id-3...].
                                  ;;
                                  ;;
                                  
                   :lines-matrix []
                   			   ;; A vector of (N-1) 1 to N-1 sized vectors where N is the grid side size, and the element value
                   			   ;; refers to a line ID in :lines which contains both cells referenced by the element's, and its containing
                   			   ;; vector's, position.
                   			   ;;
                   			   ;;
                   
                   
					:nextId 	0

}))