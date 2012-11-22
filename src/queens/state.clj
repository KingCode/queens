(ns queens.state
	(:use queens.cache))

;;cells are 1-based [x y] coordinates vectors of a location inside a 'size' sided quare grid.
;;This root binding if primarily for documentation and default grid size, and should be overriden/rebound.
;;(comment ;; REPLACING with indiviual  symbols
(def ^:dynamic state (atom {
                   :queens []     ;; occupied cells, order is important
                                  ;;
                                  ;;
                                  
                   ;;
                   ;; Used to pick the next candidate cell, which must not be in this coll.
                   ;;                                  
                   :hotcells (sorted-map) ;; cells which are currently off-limit
                                          ;; as keys, and line IDs of hotlines they belong to 
                   				  			        ;; as values.                                       				  			        
                   				  			        
                    ;;
                    ;; Used for backtracking in order to prune :hotcells                    
                    ;;
				           :hotlines (sorted-set) ;; line IDs for lines containing hot cells
				           
				           ;;
				           ;; Used for backtracking in order to lookup up and update :hotlines
				           ;;
                                  
                    :size  5      ;; the size of the grid, one-based indexing with 5 as default. Should be rebound.
               }));) 

;;
;; Module interface functions below this point
;;
(defn hotcells [] (:hotcells @state))
(defn hotlines [] (:hotlines @state))
(defn queens [] (:queens @state))
(defn lines-map [] (:lines @state))
(defn cells-map [] (:cells @state))
(defn size [] (:size @state))

(defn add-queen! [ q ]
    (let [new-queens (conj (queens) q) ]
     (reset! state (merge @state {:queens new-queens}))))

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
;; the lines defined partly by the popped queen are the only ones the pruned cells belong to.
;;
;; 
;;(defn prune-hotcells [ pq ]
		;;find all-lines pq is on:
		;; 		each baseline
;;		(let [ pq-lines 
		;;fronm pq-lines, remove
