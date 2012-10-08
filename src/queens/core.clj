(ns queens.core)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Algorithm outline:
;;          init [grid-size]: initialize (globally unbound) 'state' var.  with grid size 
;;          scan-grid: Initialize and scan in row/col order. Assign the next/first candidate cell, and verify.
;;                     If verification fails, free the candidate cell, move one cell forward and repeat.
;;                     Output a list of accepted candidate squares and the number of rows assigned.
;;          verify [cell]: for each element e  in (:occupied state), invoke (occupied-basic cell) then (occupied-lines cell) 
;;                         and return false if any of them returns true.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;cells are 1-based [x y] coordinates vectors of a location inside a 'size' sided quare grid.
;;This root binding if primarily for documentation and default grid size, and should be overriden/rebound.
(def ^:dynamic state (atom { 
                   :queens []     ;; occupied cells
                                  ;;
                   :lines  {}     ;; currently known lines (other than rows, columns, and diagonals, as a 
                                  ;; map of lines, each of which is a vector of cells [[[x1 y1][x2 y2]..] [...]]
                                  ;; to a map of 
                                  ;;    :owners -> [cell, cell] in order of addition, i.e. row then col order.
                                  ;; :owners value is updated when candidate cells are added / removed. When count is 2,
                                  ;; the line is full.
                                  ;; No entry is ever deleted to avoid having to recompute them.
                                  ;;
                    :cells {}     ;; map of cell -> vector of lines for fast lookup of lines a cell belongs to. 
                                  ;; Lines are the same as :lines keys.
                                  ;;
                    :size  5      ;; the size of the grid with 5 as default with start/end being [1 1] and [5 5]
               }))

;; Returns true if coll filtered with pred
;; contains at least one element; false otherwise.
(defmacro coll-pred [coll pred]
    `(< 0 (count (filter #(~pred %) ~coll))))     
    

(defn occupied? [[x y]]  (coll-pred (:queens @state) 
                            #(and (= x (first %)) (= y (second %))) )) 


(defn same-row? [[x1 y1] [x2 y2]] (= x1 x2))
(defn same-col? [[x1 y1] [x2 y2]] (= y1 y2))
(defn same-diag? [[x1 y1] [x2 y2]] (let [xdiff (Math/abs (- x2 x1)) ydiff (Math/abs (- y2 y1))] (= xdiff ydiff)))

;; Returns true if two cells are in the same row, column or 45 degree angle diagonal; false otherwise
(defn same-baseline? [c1 c2]
        (or (same-row? c1 c2) (same-col? c1 c2) (same-diag? c1 c2)))

;; Returns true if cell lies on the same-baseline as one of the cells in coll 
(defn same-baseline-from? [coll cell]
    (cond (same-baseline? (first coll) cell) true
          (= 1 (count coll)) false
          :else 
            (recur (next coll) cell)))

;; Wrapper around -> SEE same-baseline-from [(:queens @state) cell
(defn same-baseline-any? [cell] (same-baseline-from? (:queens @state) cell))

(defn outside-boundary? [[x y]]
    (let [size (:size @state)]
        (or (< size x) (< x 1) (< size y) (< y 1))))

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

;; Wrapper around -> SEE ABOVE line-with-acc
(defn line-with [ c1 c2] (line-with-acc c1 c2 []))
                        
;; Places a new Queen on the grid
(defn add-queen [[x y]] (swap! state #(assoc  % :queens (conj (:queens %) [x y])))) 

;; Verifies that all queens currently in (:queens @state) comply with the rules
;; Returns a map with following key/values
;;                      :queen -> position of first found queen which caused failure if any
;;                      :count -> the number of queens prior to failure point, or all of them if verifcation passes
;;                      :passed -> true if verification passed, false otherwise
(defn verify 
            ;; all current queens are assumed to have passed verification
            ([candidate] 
                { :queen nil :count 1 :passed true}))


