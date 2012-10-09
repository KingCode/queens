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
                                  ;;
                   :lines  {}     ;; Storage for 'computed' lines, i.e. those for which same-baseline?
                                  ;;  returns false b/w any two cells within them
                                  ;; and their current owners representing up to two cells occupied by queens.
                                  ;; A map of line IDs to a map with key values
                                  ;;    :cells -> a vector of all grid cells on the line
                                  ;;    :owners -> [cell, cell] in order of addition, i.e. row then col order.
                                  ;; :owners value is updated when candidate cells are added / removed. When count is 2,
                                  ;; the line is full.
                                  ;;
                                  ;; No entry is ever deleted since computation is expensive and lines are reusable within the current solution
                                  ;; and possibly beyond.
                                  ;;
                                  ;; The IDs are assumed to be created and managed in a thread-safe manner.
                                  ;;
                                  ;;
                    :cells {}     ;; map of cell -> vector of line IDs referencing keys of :lines for fast lookup of lines a cell belongs to.
                                  ;;
                                  ;;
                    :size  5      ;; the size of the grid with 5 as default with start/end being [1 1] and [5 5]. Should be rebound.
               }))
              
(defn id-generator []
    (let [id (atom 0)]
        (fn [] (swap! id inc) )))

;; Returns true if coll filtered with pred
;; contains at least one element; false otherwise.
(defmacro coll-pred [coll pred]
    `(< 0 (count (filter #(~pred %) ~coll))))    

(defn same? [[x1 y1] [x2 y2]] (and (= x1 x2) (= y1 y2)))

(defn contains-cell? [coll elem] (coll-pred coll same?)) 

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

;; Verifies independently (without changing the state) that all queens currently in (:queens @state) 
;; comply with the rules.
;; TODO

;; Verifies that 'remainder' cells comply with the rules, assuming 'compliant' cells have been verified.
;; Both arguments are assumed to be sorted in row column order. The last argument stores non-baseline
;; formations from known cells contained in them, each with two cells from 'compliant' in it.
;; If remainder is nil or empty, 'true' is returned.
;; Returns a map with following key/values
;;                      :queen -> position of first found queen which caused failure if any
;;                      :count -> the number of queens prior to failure point, or all of them if verifcation passes
;;                      :passed -> true if verification passed, false otherwise
(defn verify ([ compliant, remainder, usedlines-cells]
    (let [candidate (first remainder)]
       (cond (empty? remainder) true
          (contains-cell? usedlines-cells candidate) false
          (same-baseline-from? compliant candidate) false
          :else
            (recur (conj compliant candidate) (next remainder) (conj usedlines-cells candidate)))))

    ([] (verify [] (:queens @state) [])))

          




