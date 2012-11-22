(ns queens.str-util
	(:use queens.util))

;;
;; Generates a center-justified string from val assuming the value string has length width,
;; using padding-char as padding unit, which should be a single char or an ascii string of length 1.
;;        
(defn padd-str [val, width, padding-char]
	(let [ s (str val) len (count s) padsiz (- width len) pfx (quot padsiz 2) sfx (- padsiz pfx) ]
		(str (apply str (repeat pfx padding-char)) s (apply str (repeat sfx padding-char)))))
      
;;
;; Generates a representation of the matrix with labels 
;;
(defn format-matrix 
  ( [matrix siz default sep margin-siz]
      (let [ 
      	   margin (apply str (repeat margin-siz " ")) 
           r (triangle-rowLabels siz)
           c (triangle-colLabels siz)
           top (apply str (interpose sep c))                      
           labelled-rows (partition 2 (interleave r matrix))         
           coll (map 
         	  		(fn [e] (let [ label (first e) cells (second e)
         				      	   vals (map #(if (= nil %) default (padd-str % (count default) " ")) cells)
         					  	   formatted-vals (apply str (interpose sep vals)) ]
         				 	  (str label sep formatted-vals)))				  
              		labelled-rows)
		   lines (interpose "\n" coll)              		
          ]
          (str margin sep top "\n" (apply str lines))
          ))) 

