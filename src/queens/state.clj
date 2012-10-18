(ns queens.state)

;;cells are 1-based [x y] coordinates vectors of a location inside a 'size' sided quare grid.
;;This root binding if primarily for documentation and default grid size, and should be overriden/rebound.
;;(comment ;; REPLACING with indiviual  symbols
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
               }));) 

;;State implementation data structures
;;
(def q (atom []))               ;; queens
(def hc (atom (sorted-set []))) ;;hotcells
(def l (atom {}))               ;; lines
(def s (atom 5))                ;; size


;;
;; Module interface functions below this point
;;
(defn hotcells [] @hc)

;;
;; Adds one or more hotcells
;;
(defn add-hotcells 
            ( [ acc newcells ]  
                    (if (empty? newcells) acc 
                                    (recur (conj acc (first newcells)) (next newcells)))) 
            ( [ newcells ]
                    (let [ merged (add-hotcells hotcells newcells) ]
                        (reset! hc merged))))

;;
;; Prunes hotcells  
;; 
;;(defn prune-hotcells
