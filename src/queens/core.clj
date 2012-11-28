(ns queens.core
	(:use queens.util queens.state queens.cache))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions: a baseline is a row, column or diagonal to a 45 degree angle. Otherwise a line is 'irregular'.
;;
;; Problem: compute coordinates of N locations on a square grid of NxN locations such that: 
;;          - no more than one of them can be on the same baseline
;;          - no more than two of them can be on any other line
;;
;; Algorithm outline for the first found solution:
;;          INPUTS: side length size, denoted as the size
;;          DATA STRUCTURES:
;;                  LOOKUP - an updateable lookup of line IDs and corresponding cells
;;                    for any pair of cells in the grid. The only updates are
;;                    inserts.Line IDs are generated internally by the lookup.
;;                  STATE - an immutable state passed as argument with the following
;;                    key-values:
;;                      :size, as per above
;;                      :queens, a vector of candidate cells so far
;;                      :hotcells, a vector of cells off-limit for future candidates
;;                      :hotlines, a map of lineIDs containing all cells in :hotcells
;;                      :queens2lines, a map of cells to a vector of ine IDs with each
;;                                     element of :queens as keys
;;                      :last-allowed, a duple of an index to :queens and the 
;;                                     cell value at that position beyond which
;;                                     the current search ends.
;;                      :cursor the cell to be considered next
;;
;; Routine INIT [ size ] initialize global LOOKUP and local STATE with size  
;;
;; Function FILL-QUEENS [ state ]
;; 1. If 
;;      1-a. all queens have been found or 
;;      1-b. the tail element of :queens is :last-allowed > index AND
;;           :cursor is past :last-allowed > value we're done
;;      THEN we are done. Return the input state
;; 2. If :cursor is outside the boundary, backtrack (see below)
;; 3. If the position at cursor is found in :hotcells, increment cursor and recurse.
;; 4. Else the cursored position is added to the candidate solution:
;;   4.1 Add it to :queens
;;   4.2 Add all baselines the new queen occupies, and all newly formed lines with
;;      previous queens, to :hotlines 
;;   4.3 Create a new entry in :queens2lines with the new queen as key and new lines as a vector if IDs
;;   4.4 Add all cells in the new lines to :hotcells
;;   4.5 Increment ;cursor and recurse wih the new state
;;
;;      NOTE: the use :last-allowed is currently static, e.g. would always be (1 [1 :size]).
;;            The intent is to eventually use it to demarcate concurrency boundaries.
;;
;;  Function BACKTRACK [ state ]
;;  1. Pop off the last added queen Q
;;  2. Retrieve all lines occupied by Q: 
;;      2.1 fetch hotline IDs from :queens2lines (QL)
;;      2.2 remove POQ entry from :queens2lines
;;  3. Remove from :hotlines the QL. This is so because a line with two queens is not a baseline, 
;;      and a baseline becomes free when its (only) occupied cell is freed.
;;  4. Remove from :hotcells the QL cells which do not belong to any remaining hot line:
;;     4.1 For each candidate for removal,
;;         4.1.1 lookup all the lines it belongs in LOOKUP
;;         4.1.2 perform intersection with the remaining hot lines.
;;               If intersect is empty the cell can be removed              
;;  5. Move cursor to (nextpos Q) and call FILL-QUEENS with the new state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defn next-candidate [ state ] 
    (let [ 
    		queens (:queens state)
    		from (last queens)
    		size (:size state)
    		cursor (:cursor state)    		
    		nextcursor (next-irregular from cursor size)
    	 ]
        (assoc state :cursor nextcursor)))  

;;
;; Returns a map of lineID -> [coll candidate-cell] entries
;; for each pairing b/w one of coll and candidate.
;;
(defn new-line-ids [coll candidate]
  (apply merge (map #({(line-id % candidate) [% candidate]}) coll)))

;;
;; 
;; (defn solution    
  

;;
;; Returns a set of line IDs for baselines around cell
;;
(defn get-baseline-ids [cell]
  (let [ around (surrounding-cells cell (:size state)) ]
    (set (map #(line-id % cell) around))))
 
;;
;;
;;
(defn hole-in-queens? [ queens candidate ]
    (if (empty? queens) false
        (let [ [x y] (last queens) [c d] candidate ]
            (> c (inc x))))) 

;;
;;
;;
(defn candidate-exhausted? [ state ]
    (let [ cursor (:cursor state)
           queens (:queens state) la (:last-allowed state)
           la-index (first la) la-val (second la) ]

        (and (= (inc la-index) (count queens)) (> 0 (compare la-val cursor)))))

(declare backtrack)
    
;;
;; Examines cell 'cursor' and if eligible to become a queen, updates the state,
;; and returns a function invoking candidate-from (next-candidate cursor).
;; If not found, and the number of queens is < N, a function invoking backtrack is returned.
;;
;; ALGORITHM:
;; - If all queens have been found we're done; return the input state
;; - Else if cursor is outside the boundary:
;;   -- If the first queen in the candidate solution is no longer in the first row,
;;      there is no solution possible that hasn't been tried. Return the state with :failure set.
;;   -- Else, backtrack (see below).
;; - If the position at cursor is found in :hotcells, increment cursor and recurse.
;; - Else the cursored position is added to the candidate solution:
;;   -- Add it to :queens
;;   -- Add the four baselines the new queen occupies, and all newly formed lines with
;;      previous queens, to :hotlines 
;;   -- Create a new entry in :queens2lines with the new queen as key and new lines as a vector if IDs
;;   -- Add all cells in the new lines to :hotcells
;;   -- Increment the cursor and recurse
;;
(comment
(defn fill-queens [ state ]
    (let [ hc (:hotcells state) queens (:queens state) siz (:size state)
           cursor (:cursor state) hl (:hotlines state)
           q2l (:queens2line state)
          ]
	    (cond 	
            (= siz (count queens)) #(self state)    ;; solution found: we're done
            (candidate-exhausted? state) #(self state) ;; no more solutions
            (hole-in-queens? :queens cursor)  #(backtrack) ;; no more candidates with current set of queens: backtrack

            ;; cursored candidate invalid: move on to the next one
            (in hc cursor) #(fill-queens (next-candidate state))

            :else 
                ;; cursored candidate valid:
                ;; add it to queens
                ;; add all newly-formed lines' cells to :hotcells
                ;; then move cursor forward and repeat

                (let [ 
                       neighbours (surrounding-cells cursor siz)
                       newBLids (new-line-ids neighbours cursor siz)
                       newIds (new-line-ids queens cursor) 
                       newLines (map #(getLine %) (set (concat newBLids newIds)))
                       newHCs (apply sorted-map (interleave newIds newLines))
                       updatedLines                     
                       updatedHCs (apply merge hc newHCs)
                       updatedQueens (conj queens cursor)                       
                     ]
                    (add-queen! cursor)
                      #(fill-queens nextpos)))))  
)
;;
;; Removes the last added queen and all related hot cells, and returns a fn invoking fill-queens-from
;; with the updated state. If all cells in the first row have been tried a fn returning nil is returned.
;;
;; ALGORITHM: 
;;    - Pop off the last added queen Q
;;    - Retrieve all lines occupied by Q: 
;;      -- fetch hotline IDs from :queens2lines (QL)
;;      -- remove POQ entry from :queens2lines
;;    - Remove from :hotlines the QL. This is so because a line with two queens is not a baseline, 
;;      and a baseline becomes free when its (only) occupied cell is freed.
;;    - Remove from :hotcells the QL cells which do not belong to any remaining hot line:
;;      -- For each candidate for removal,
;;         --- Lookup all the lines it belongs to (fast lookup) and perform intersection with 
;;             the remaining hot lines. If intersect is empty the cell can be removed              
;;    - Increment cursor and
;;    - Yield a function looking for the next eligible candidate with updated state and cursor      
;; (defn backtrack [] 
;;    (let [ q (pop (:queens @state)) 
;;            qn (inc-pos q)          ]  
;;       (if (empty? qn)  #(self nil)
          ;;prune :hotcells 
