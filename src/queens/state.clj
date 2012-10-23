(ns queens.state)

;;cells are 1-based [x y] coordinates vectors of a location inside a 'size' sided quare grid.
;;This root binding if primarily for documentation and default grid size, and should be overriden/rebound.
;;(comment ;; REPLACING with indiviual  symbols
(def ^:dynamic state (atom {
                   :queens []     ;; occupied cells
                                  ;;
                                  ;;
                   :lines  	 (hash-map)
                   			      ;; a cache of pre-computed lines, as a map of line IDs to a (sorted ) vector of all grid cells
                                  ;; on the line: integer -> [[x1 y1] [x2 y2]...]
                                  ;;
                                  ;; The IDs are assumed to be unique and implement hashCode().
                                  ;;
                   :cells 	 (sorted-map)
                    		      ;; for fast lookup in :lines, of an existing line formation map of cells to line IDs used as
                                  ;; keys to :lines. [x y] -> [line-id-1 line-id-2 line-id-3...].
                                  ;;
                                  ;;
                   :hotcells  (sorted-set)  ; cells which are off-limit for the next queen
                   				  ;; 
                   				  ;;
                                  
				   :hotlines (hash-map) ;; mappings of line IDs -> mapping of :queens and :is-baseline keys 
				   						;; -> (vector) tuple of queen cells and true/false
				   						;;
				   						;; line IDs are consistent and equal to those in :lines
                                  
                    :size  5      ;; the size of the grid with 5 as default with start/end being [1 1] and [5 5]. Should be rebound.
               }));) 

;;State implementation data structures

;; queens - stores the solution candidates
(def q (atom (sorted-set [])))

;; hot cells - not eligible for occupation by a queen
(def hc (atom (sorted-set []))) 

;; all lines formation known - as a cache to avoid recomputation
(def l (atom {}))

;; all lines storing hot cells
(def hl (atom {}))

;;
(def s (atom 5))


;;
;; Module interface functions below this point
;;
(defn hotcells [] (:hotcells @state))
(defn hotlines [] (:hotlines @state))
(defn queens [] (:queens @state))
(defn lines-map [] (:lines @state))
(defn cells-map [] (:cells @state))
(defn size [] (:size @state))

;;
;; Adds one or more hotcells
;;
(defn add-hotcells! 
            ( [ acc newcells ]  
                    (if (empty? newcells) acc 
                                    (recur (conj acc (first newcells)) (next newcells))))
            ( [ newcells ]
                    (let [ merged (add-hotcells! (hotcells) newcells) ]
                        (reset! state (merge @state {:hotcells merged})))))

;;
;; Adds a line to the 
;;


;;
;; Prunes hot cells which should no longer be hot due to by removing the argument queen (popped queen), i.e.
;; the lines defined partly by the popped queen are the only ones the pruced cells belong to.
;;
;; 
;;(defn prune-hotcells [ pq ]
		;;find all-lines pq is on:
		;; 		each baseline
;;		(let [ pq-lines 
		;;fronm pq-lines, remove
