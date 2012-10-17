(ns queens.core
	(:use queens.util))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions: a baseline is a row, column or diagonal to a 45 degree angle. Otherwise a line is 'irregular'.
;;
;; Problem: compute coordinates of N locations on a square grid of NxN locations such that: 
;;          - no more than one of them can be on the same baseline
;;          - no more than two of them can be on any other line
;;
;; Algorithm outline for the first found solution:
;;          init [grid-size]: initialize (globally unbound) 'state' var.  with grid size
;;          scan-grid:
;;                0) Initialize 
;;                1) Pick the next candidate.
;;                    1.1) If the next candidate is nil, go to step 3
;;                    1.2) If candidate is one of :hotcells return to step 1
;;                        1.3) Else, create all irregular lines b/w candidate and each of :queens and add it to :lines
;;                            1.3.1) add all newly created line's elements to :hotcells
;;                            1.3.2) add candidate to :queens
;;                2) If :queens has N elements the solution is complete, else repeat step 1.
;;                3) If :queens has less than N elements AND there are no more candidates, backtracking is required.
;;                    3.1) if queens is empty and all candidate queens on the first row have been tried, then there
;;                         is no solution for size N. Else,
;;                    3.2) remove the last element from :queens.
;;                    3.3) for all lines containing the removed element and no other from :queens
;;                        3.2.1) remove corresponding elements within them, from :hotcells
;;                    3.4) repeat step 1.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;cells are 1-based [x y] coordinates vectors of a location inside a 'size' sided quare grid.
;;This root binding if primarily for documentation and default grid size, and should be overriden/rebound.
(def ^:dynamic state (atom {
                   :queens []     ;; occupied cells
                                  ;;
                                  ;;
                   :hotcells  (sorted-set [])  ; cells, anyone of which being occupied would invalidate the current candidate,
                                  ;; gets updated after each candidate evaluation. Sorted for binary search and fast
                                  ;; updates.
                                  ;;
                   :lines  {}     ;; a cache of computed lines for reuse, as a map of line IDs to a (sorted ) vector of all grid cells
                                  ;; on the line: integer -> [[x1 y1] [x2 y2]...]
                                  ;;
                                  ;; The IDs are assumed to be created and managed in a thread-safe manner.
                                  ;;
                                  ;; The intended use is to lookup a line formation in the cache in order to update :hotcells
                                  ;;
                                  ;;
                    :cells {}     ;; for fast lookup in :lines, of an existing line formation map of cells to line IDs used as
                                  ;; keys to :lines. [x y] -> [id-1, id-2, id-3...].
                                  ;;
                                  ;;
                    :size  5      ;; the size of the grid with 5 as default with start/end being [1 1] and [5 5]. Should be rebound.
               }))              

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
                       
;; Places a new Queen on the grid
(defn add-queen [[x y]] (swap! state #(assoc  % :queens (conj (:queens %) [x y]))))

;; Returns complete lines defined by each element of 'coll' and 'cell'
(defn lines-between [ coll cell ] 
	(map #(line-with % cell) coll))


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


;;
;; Returns the next position following the argument location.
;;
(defn inc-cursor [ pos ] (move-1 pos (:size @state)))
    
    
;;
;; 
;; (defn solution    
    
(declare backtrack)
    
;;
;; Examines cell 'cursor' and if eligible to become a queen, updates the state,
;; and returns a function invoking candidate-from (inc-cursor cursor).
;; If not found, and the number of queens is < N, a function invoking backtrack is returned.
;;
(comment
(defn candidate [ cursor ]
    (let [ nextpos (inc-pos cursor) ]
	(cond 
             (= (:size @state) (count (:queens @state))) #(self DONE)
             (= nil cursor) #(backtrack)
             (contains-cell? (:hotcells @state) cursor)  #(candidate nextpos)
             :else   
                    ;; add cursor to :queens, then move cursor forward and repeat
                    (let [ newlines (lines-between (:queens @state) cursor) ]
                        ;; add to :hotcells all cells in saturated lines
                         (conj (:queens @state) cursor)
                         #(candidate nextpos)))))  
)
;;
;; Removes the last added queen and all related hot cells, and returns a fn invoking next-candidate
;; or, if all cells in the first row have been tried a fn returning DONE
;;
;; (defn backtrack [] 
;;    (let [ q (pop (:queens @state)) 
;;            qn (inc-pos q)          ]  
;;       (if (empty? qn)  #(self DONE)
          ;;prune :hotcells 
