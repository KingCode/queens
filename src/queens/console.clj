(defmacro seqStr [seq]
`(apply str ~seq))

(defn margin-for [wordSiz sep] (str (seqStr (repeat wordSiz " ")) sep))

(def config (atom { :size 5 :sep "   " :vsep "\n\n" :padding " " :paddedSize 1 :margin (margin-for 1 "   ") :filled "Q" :empty "*"}))

;;shortcuts for reading config props
(defn size[] (:size @config))
(defn sep [] (:sep @config))
(defn vsep [] (:vsep @config))
(defn padding [] (:padding @config))
(defn paddedSize [] (:paddedSize @config))
(defn margin [] (:margin @config))
(defn filled [] (:filled @config))
(defn free [] (:empty @config))

;;shortcuts to update config props
(defn size! [newSiz] (swap! config (fn [m]
                                        (let [update-1 (assoc m :size newSiz)
                                              update-2 (assoc update-1 :paddedSize (.length (str (:size update-1)))) ]
                                              (assoc update-2 :margin (margin-for (:paddedSize update-2) (:sep update-2)))))))

(defn sep! [newSep] (swap! config (fn [m] (assoc m :sep newSep))))
(defn vsep! [newVSep] (swap! config (fn [m] (assoc m :vsep newVSep))))
(defn padding! [newPadding] (swap! config (fn [m] (assoc m :padding newPadding))))
(defn filled! [newFilled] (swap! config (fn [m] (assoc m :filled newFilled))))
(defn empty! [newEmpty] (swap! config (fn [m] (assoc m :empty newEmpty))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following are better left alone - autoset within (size!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn paddedSize! [newPaddedSize] (swap! config (fn [m] (assoc m :paddedSize newPaddedSize))))
(defn margin! [newMargin] (swap! config (fn [m] (assoc m :margin newMargin))))

(defn grid [size]
    (for [row (range 1 (inc size)) col (range 1 (inc size))]
        [row col])
)

;;(defn board [](grid (Long/parseLong size)))
(defn board [](grid (size)))

(def queens (atom []))

(defn add [queen]
    (swap! queens conj queen))
   
(defn occupied? [row col]
   (< 0 (count (filter #(and (= row (first %)) (= col (second %))) @queens)) ))          

(defn maxfirst [a b] (if (< a b) [b a] [a b]))

(defn pad [content]
    (let [c (str content) len (.length c) paddingSiz (- (paddedSize) len)]
        (cond (<= (paddedSize) len)
                c
              (even? paddingSiz)
                  (let [endSiz (/ paddingSiz 2) stuffing (repeat endSiz (padding)) end (seqStr stuffing)]
                      (str end c end))
              :else
                  (let [    end1 (quot paddingSiz 2) end2 (- paddingSiz end1)
                          order (maxfirst end1 end2)
                          pfxSiz (first order) sfxSiz (second order)
                          pfxSeq (repeat pfxSiz (padding)) pfx (seqStr pfxSeq)
                          sfxSeq (repeat sfxSiz (padding)) sfx (seqStr sfxSeq)
                      ]
                      (str pfx c sfx)))))

(defn col-labels []
    (apply str (interpose (sep) (map #(pad %) (range 1 (inc (size)))))))   

(defn col-headers []
    (apply str (margin) (col-labels)))   
   
(defn col-footers []
    (apply str (margin) (col-labels)))
       
(defn cell-content [[x y]] (if (occupied? x y) (filled) (free)))   

(defn cell [pair] (pad (cell-content pair)))
   
(defn board-str []
    (apply str (map (fn [[x y]]
            ;;headers for rows and/or columns
            (let [pfx
                        (cond (and (= 1 y) (= 1 x))    (str (col-headers) (vsep) (pad x) (sep) )
                          (= 1 y) (str (pad x) (sep))
                          :else "")
                  sfx
                          (cond (and (= (size) y) (= (size) x)) (str x (vsep) (col-footers))
                                (= (size) y) (str x (vsep))
                           :else "")
                  ]
                (str pfx (cell [x y]) (sep) sfx))) (board))))
               
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;               

(defn toNums [[i j]] [ (Integer/parseInt i) (Integer/parseInt j)])

(defn add-queen [pair] (add (toNums pair)))

(defn parse-queens [arg]
        (let [arrays (for [x (.split (str arg) ", ")] (.split x " "))
              pairs (map (fn [p] [(nth p 0) (nth p 1)]) arrays)]
            (doseq [pos pairs] (add-queen pos))))
           
(defn clear-queens [] (reset! queens {}))           

(defn back-queens [] (swap! queens #(if (not (empty? %)) (pop %))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop []
    (println "\nEnter grid size or 'a'/'c' to add or clear all queens ('q' to quit): ")
    (let [val (read-line)]
        (cond (= val "q") (System/exit 0)
            (= val "a") (do (println "Enter queens to add, e.g. 1 2, 3 4 to add [1 2] and [3 4]")
                            (parse-queens (read-line))
                            (println (board-str)))
            (= val "b") (do (back-queens) (println (board-str)))
            (= val "c") (do (clear-queens)
                            (println (board-str)))
            (= val "") (println (board-str))
            (not (.matches val "\\d+")) (println "You must enter a number!")
            :else  (do (size! (Integer/parseInt val)) (println (board-str))))
            ;;:else  (println "Got a number!")
            (recur) ))
    
