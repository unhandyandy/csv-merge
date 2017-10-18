(ns csv-merge.core
  (:gen-class)
  (:use clojure-csv.core
        seesaw.core
        seesaw.chooser
        flatland.ordered.map
        ))

(native!)

(def addcsv (atom nil))
(def basecsv (atom nil))
;;(def rmkcsv (atom nil))
(def adddict (atom {}))
(def basedict (atom {}))
;;(def rmkdict (atom {}))
(def cols (atom '()))
;;(def colsmap (atom {}))
(def drop-cols 6)
(def load? (atom true))
(def keytab (atom []))
(def los-list nil)
(def los-hash {})
(def key-ver-pos nil)
(def partial-credit? nil)
(def min-lo-score 0.6)
(def num-choices 8)
(def curve-frame (atom nil))
(def make-funs (atom {["base" nil] ["add" nil]}))


(defn rm-parens [str]
  (if (= str "") ""
      (clojure.string/trimr (re-find #"^[^(]+" str))))

(defn postjoin [l e]
  (reverse (conj (reverse l) e)))

(defn check-ww [tab]
  (let [[[e11 e12] [e21] [e31]] tab]
    (and (= e11 "NO OF FIELDS")
         (= e12 "")
         (= e21 "SET NAME")
         (= e31 "PROB NUMBER"))))

(defn check-cnv [tab]
  (let [[[e11 e12] [e21] [e31]] tab]
    (and (= e11 "Student")
         (= e12 "ID")
         (or (and (= e21 "")
                  (re-matches #"\s*Points Possible" e31))
             (re-matches #"\s*Points Possible" e21)))))

(defn check-remark [tab]
  (let [[[e11 e12 e13] [e21] [e31]] tab
        tests (map #(< (count %) 2) (list e11 e12 e13 e21 e31))]
    (every? true? tests)))

(defn clean-blanks [str]
  (map #(if (> (count %) 1) "" %) str))

(defn mk-line-readable [v n]
  (let [nameletters (clean-blanks (subvec v 0 8))
        sectiondigits (clean-blanks (subvec v 8 13))
        codedigits (clean-blanks (subvec v 13 19))
        uname (clojure.string/trim (clojure.string/lower-case (clojure.string/join nameletters)))
        code (clojure.string/join (map str codedigits))
        section (clojure.string/join (map str sectiondigits))]
    (vec (concat [uname section code] (subvec v 19)))))

(defn mk-readable [tab]
  (let [numq (- (count (first tab)) 19)
        qseq (map #(clojure.string/join (list "EQ" (str %))) (range 1 (inc numq)))
        header (concat ["username" "section" "examcode"] qseq)]
    (concat [(vec header)] (map #(mk-line-readable % numq) tab))))


;; (defn col-match [col1 col2]
;;   (let [len1 (count col1)
;;         len2 (count col2)]
;;     (if (> len1 len2) false
;;       (let [trunc (subs col2 0 len1)]
;;         (= col1 trunc)))))

(defn col-match [col1 col2]
  (let [trunc1 (rm-parens col1)
        trunc2 (rm-parens col2)]
    (= trunc1 trunc2)))

(defn tab->mapmap [[cols & rows] idindex start]
  (let [dict (atom {})]
    (doseq [r rows]
      (let [id (nth r idindex)
            twist (map #(list (rm-parens %1) %2) cols r)
            rowdict (apply ordered-map (apply concat twist))]  
        ;; (doseq [[c s] (drop start twist)]
        ;;   (swap! rowdict assoc c s))
        (swap! dict assoc id rowdict)))
    @dict))

(defn update-text [])

(defn make-rmkdict
  ([]
   (let [tabmod @addcsv]
     (reset! adddict (tab->mapmap tabmod 0 1))
     (update-text)))
  ([targ]
   (let [targatom (if (= targ "base") basedict adddict)
         targcsv (if (= targ "base") @basecsv @addcsv)]
    (reset! targatom (tab->mapmap targcsv 0 1))
    (update-text))))

(defn make-wwdict
  ([]
   (reset! adddict (tab->mapmap (drop 1 @addcsv) 1 drop-cols)))
  ([targ]
   (let [targatom (if (= targ "base") basedict adddict)
         targcsv (if (= targ "base") @basecsv @addcsv)]
    (reset! targatom (tab->mapmap (drop 1 targcsv) 1 drop-cols))
    (update-text))))

(defn make-cnvdict [targ]
  (let [targatom (if (= targ "base") basedict adddict)
        targcsv (if (= targ "base") @basecsv @addcsv)]
    (reset! targatom (tab->mapmap targcsv 2 0))))

(defn update-dict [targ]
  (let [makefun (@make-funs targ)]
    (makefun targ)))

;; (defn get-rmkcols []
;;   ;;(drop 1 (first @rmkcsv))
;;   (keys (@rmkdict "")))

(defn get-addcols []
  ;;(drop drop-cols (second @addcsv))
  (keys (second (first @adddict))))

(defn get-basecols []
  ;;(first @basecsv)
  (keys (@basedict "")))

(defn find-first-match [item coll]
  (first (drop-while #(not (col-match item %)) coll)))

;; (defn make-colsmap []
;;   (let [addcols (get-addcols)
;;         basecols (get-basecols)]
;;     (reset! colsmap {})
;;     (doseq [c addcols]
;;       (let [m (find-first-match c basecols)]
;;         (if m
;;           (swap! colsmap assoc c m)
;;           (swap! cols postjoin c))))))

(def control-frame)
(defn dict->canvas [] )

(defn keys-add-col
  "Add first column to @keytab showing index"
  [tab]
  (map #(into [(str %1)] %2)
       (range 10)
       tab))

(defn update-text []
  (let [cnvfd (select control-frame [:#base-text])
        tomergefd (select control-frame [:#add-text])
        keyfd (select control-frame [:#key-text])
        ;;tomergetxt (if @addcsv @addcsv @rmkcsv)
        ]
    (reset! basecsv (dict->canvas @basedict))
    (text! cnvfd (if @basedict (write-csv @basecsv) ""))
    (text! tomergefd (if @adddict (write-csv @addcsv) ""))
    (text! keyfd (if @keytab (write-csv @keytab) ""))))

(defn get-caret-pos-in
  "get position 0f cursor in the named text frame."
  [fr]
  (let [f (if (= fr "base")
            (select control-frame [:#base-text])
            (if (= fr "add")
              (select control-frame [:#add-text])
              (select control-frame [:#key-text])))]
    (-> f .getCaretPosition)))

(defn get-line-num-in
  "get line number in given text of given cursor pos"
  [txt pos]
  (count (re-seq #"\n" (subs txt 0 pos))))

(defn get-username-in-rmk
  "get the username of the person where the cursor currently is in the add field, if that field is a remark file"
  []
  (when (= (@make-funs "add") make-rmkdict)
    (let [pos (get-caret-pos-in "add")
          txt (text (select control-frame [:#add-text]))
          line (get-line-num-in txt pos)]
      (first (nth @addcsv line)))))

(defn update-cols []
  (let [addcols (get-addcols)
        basecols (get-basecols)]
    (doseq [c (concat basecols addcols)]
      (if-not (some #{c} @cols) 
        (swap! cols postjoin c)))))

(defn clear-all-data []
  (reset! basecsv nil)
  (reset! addcsv nil)
  (reset! adddict {})
  (reset! basedict {})
  (reset! cols '())
  (reset! keytab [])
  (def los-list nil)
  (def los-hash {})
  (def key-ver-pos nil)
  (def partial-credit? nil)
  (update-text))

(defn merge-row [row id]
  (when (@basedict id)
    (let [olddict (atom (@basedict id))]
      (doseq [[k v] row]
        (swap! olddict assoc k v))
      (swap! basedict assoc id @olddict))))

(def control-frame)

(def dict->canvas)

(defn load-csv [targ]
  (let [csvatom (if (= targ "base") basecsv addcsv)
        dicttarg (if (= targ "base") "base" "add")
        loadfile (choose-file :type :open
                              :selection-mode :files-only
                              ;; :dir (-> (java.io.File. ".") .getCanonicalPath)
                              :cancel-fn (fn [e] (reset! load? false)))]
    (when loadfile
      (let [csvstr (slurp loadfile)
            csvtab (parse-csv csvstr)]
        (reset! csvatom csvtab)
        (if (check-ww csvtab)
          (do (swap! make-funs assoc targ make-wwdict)
              (make-wwdict))
          (if (check-cnv csvtab)
            (do (swap! make-funs assoc targ make-cnvdict)
                (make-cnvdict dicttarg))
            (if (check-remark csvtab)
              (do (swap! make-funs assoc targ make-rmkdict)
                  (reset! csvatom (mk-readable csvtab))
                  (make-rmkdict))
              (println "Unknown type of csv file.")
              )))
        (update-cols)
        ;;(make-colsmap)
        (update-text)
        ))))

(defn merge-dicts
  ([] (merge-dicts "col"))
  ([rorc]
   (update-cols)
   (let [dict @adddict]
     ;;(println "here")
     (doseq [[id row] dict]
       (if (= rorc "row")
         (when (nil? (@basedict id))
           (swap! basedict assoc id row))
         (merge-row row id)))
     (reset! basecsv (dict->canvas @basedict))
     (update-text))))

;; (defn dict->canvas [dict]
;;   (let [ids (sort-by #((@basedict %) "Student") (keys dict))
;;         [_ [test :as status]] @basecsv]
;;     (loop [tab (if (= test "")
;;                  (list status @cols)
;;                  (list @cols))
;;            [id & idrem] ids]
;;       (let [newrow (loop [row '()
;;                           [col & colrem] @cols]
;;                      (let [row+  (conj row (or ((dict id) col) ""))]
;;                        (if colrem
;;                          (recur row+ colrem)
;;                          (vec (reverse row+)))))
;;             newtab (conj tab newrow)]
;;         (if idrem
;;           (recur newtab idrem)
;;           (reverse newtab))))))

(defn make-csv-row
  "Make csv row from dict corresponding to key id and columns cols"
  [id cols dict]
  (loop [row '()
         [col & colrem] cols]
    (let [row+  (conj
                 row
                 (if (@basedict id)
                   (or ((dict id) col) ((@basedict id) col) "")
                   (if (= col "SIS Login ID")
                     id
                     (or ((dict id) col) ""))))]
      (if colrem
        (recur row+ colrem)
        (vec (reverse row+))))))

(defn make-csv-tab
  "Make csv table from dict, ids, cols, initial table tab0"
  [ids cols dict tab0]
  (loop [tab tab0
           [id & idrem] ids]
      (let [newrow (make-csv-row id cols dict)
            newtab (conj tab newrow)]
        (if idrem
          (recur newtab idrem)
          (reverse newtab)))))

(defn dict->canvas [dict]
  (let [ids (sort-by #(let [d (@basedict %)]
                        (if d (d "Student") "A"))
                     (filter #(> (count %) 0) (keys dict)))
        [_ [test :as status] pp] @basecsv
        dictcols (keys (second (first dict)))
        tab0 (if (= test "")
               (list  pp status dictcols)
               (list  pp dictcols))]
    (make-csv-tab ids dictcols dict tab0)
    ))

;;convert remark format dict to canvas format dict
(defn rmk->canvas 
  "convert remark format dict to canvas format dict"
  [dict]
  (let [ids (sort-by #(let [d (@basedict %)]
                        (if d (d "Student") "A"))
                     (filter #(> (count %) 0) (keys dict)))
        sorteddict (sort-by #(count (second %)) < dict)
        dictcols (concat (take 4 @cols)
                         (drop 2 (keys (second (last sorteddict)))))]
    (make-csv-tab ids dictcols dict (list dictcols))))
  

(defn save-csv [csvstr]
  (let [savefile (choose-file :type :save
                              ;;:filters ["Text" ["txt"]]
                              )
        clobber (atom false)]
    (if (.exists  savefile)
      (let [dial (dialog :content (str "File " savefile " exists, do you want to overwrite it?")
                         :success-fn (fn [e] (reset! clobber true))
                         :type :warning
                         :option-type :yes-no)]
        (pack! dial)
        (show! dial))
      (reset! clobber true))
    (if (and savefile clobber)
      (spit savefile csvstr))))

(defn read-key []
  (let [keyfile (choose-file :type :open
                             :selection-mode :files-only)]
    (when keyfile
      (let [keystr (slurp keyfile)
            keyexp (atom keystr)]
        (while (< (count (re-seq #"\n" @keyexp)) 10)
          (swap! keyexp str keystr))
        (let [key1 (parse-csv @keyexp)
              key2 (take 10 key1)
              key3 (keys-add-col key2)]
          (reset! keytab (vec key3))))
      (update-text))))

(defn lookup-in-key [v q]
  (get-in @keytab [v (inc q)]))

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn parse-multi-str [ans]
  (if (= ans "BLANK")
    []
    (let [len (count ans)]
      (if (= len 1)
        [ans]
        (let [trimmed (subs ans (- len 1))]
          (clojure.string/split trimmed #","))))))

(defn score-one [ver prob ans]
  (if partial-credit?
    (let [pts (log2 num-choices)
          parsed (parse-multi-str ans)
          len (count parsed)
          val (if (= len 0) 0 (- pts (log2 len)))
          corans (lookup-in-key ver prob)]
      (if (some #{corans} parsed) val (- val)))
    (if (= (lookup-in-key ver prob) ans) 1 0)))

(defn grade-one [v ansvec]
  (let [numq (count ansvec)
        items (range 1 (inc (count ansvec)))
        scores (map #(score-one v %1 %2) items ansvec)
        total-score (apply + scores)
        scoreslo (map #(nth scores (dec %)) los-list)
        rightlo (filter #(>= % min-lo-score) scoreslo)
        nclo (count rightlo)]
    (when los-list
      (def los-hash (update los-hash nclo inc))
      (def los-hash (update los-hash "total" inc))
      (doseq [q los-list]
        (when (>= (nth scores (dec q)) min-lo-score)
          (def los-hash (update los-hash (str "Q" q) inc)))))
        total-score))
                   
(defn grade-exam [codepos]
  (if (> (count @keytab) 0)
    (let [len (- (count (first @addcsv)) 2)
          extot (* len (log2 num-choices))
          items (range 1 len)
          headers (conj (first @addcsv) "This Exam Score")
          newtab (atom [])]
      (swap! cols postjoin "This Exam Score")
      (when los-list
        (doseq [q los-list]
          (def los-hash (assoc los-hash (str "Q" q) 0)))
        (doseq [n (range (inc (count los-list)))]
          (def los-hash (assoc los-hash n 0)))
        (def los-hash (assoc los-hash "total" 0)))
      (doseq [[_ _ c & ans :as row] (rest @addcsv)]
        (if (> (count  c) 5)
          (let [v (read-string (subs c codepos (inc codepos)))
                ansvec (vec ans)
                s (grade-one v ansvec)
                score (if partial-credit?
                        (+ extot s)
                        s)
                newrow (conj row (str score))]
            (swap! newtab conj newrow))
          (swap! newtab conj row)))
      (reset! addcsv (concat [headers] @newtab))
      (update-dict "add")
      ;;(make-rmkdict)
      (merge-dicts "col")
      (update-text))
    (alert "No answer key loaded."
           :type :error)))

(defn grade-exam-init []
  (let [numq (- (count (first @addcsv)) 3)
        qseq (range 1 (inc numq))
        codepos (if key-ver-pos key-ver-pos
                    (input "Examcode position, 0-based: " :choices [0 1 2 3 4 5]))
        los (if los-list los-list
                (input "Learning Outcomes?"))
        partial (if (not (nil? partial-credit?)) partial-credit?
                    (input "Partial credit? " :choices ["no" "yes"] :value "no"))]
    (when (not los-list)
      (def los-list (if (> (count los) 0)
                      (map read-string (clojure.string/split los #","))
                      [])))
    (when (not key-ver-pos)
      (def key-ver-pos codepos))
    (when (not partial-credit?)
      (def partial-credit?
        (case partial "yes" true "no" false false)))
    (grade-exam codepos)))
        
(defn hash->csv [hash]
  (map (fn [[k v]] [(str k) (str v)]) los-hash))

(defn get-scores [col]
  (map #((@basedict %) col) (rest (keys @basedict))))

(defn parse-to-number-list [l]
  (let [nums (filter #(and % (re-matches #"[0-9.]+" %)) l)]
    (map #(Float/parseFloat %) nums)))

(defn mk-histogram [nums]
  (let [hist (atom {})]
    (doseq [n nums]
      (let [v (@hist n)]
        (if v
          (swap! hist assoc n (inc v))
          (swap! hist assoc n 1))))
    ;;(println (count @hist))
    @hist))

(defn fill-hist
  "fill in missing values in histogram (with 0s)"
  [hist]
  (let [ks (keys hist)
        min (apply min ks)
        max (apply max ks)
        newhist (atom hist)]
    (doseq [i (range min (inc max))]
      (when (nil? (hist (float i)))
        (swap! newhist assoc i 0)))
    @newhist))

(defn hist->csv
  "convert histogram hash to csv string"
  [hist]
  (let [hh (into (sorted-map) hist)
        csv (atom "")]
    (doseq [[k v] hh]
      (swap! csv str "\n" (str k) "," (str v)))
    @csv))

(defn mk-percentiles [nums]
  (let [res (atom [0])]
    (doseq [n nums]
      (swap! res conj (+ (last @res) n)))
    (let [tots (rest @res)
          sum (last @res)]
      (map #(float (* 100 (/ % sum))) tots))))

(defn mk-txt [str]
  (label :text str
         ;;:editable? false
         :size [80 :by 30]
         :halign :center))

(defn interpolate [curve raw]
  (if (< (count curve) 2)
    raw
    (let [left (atom (first curve))
          right (atom (first (rest curve)))]
      (loop [todo (rest (rest curve))]
        (when (and (first todo) (>= raw (first @right)))
          (reset! left @right)
          (reset! right (first todo))
          (when (> (count todo) 1)
            (recur (rest todo)))))
      (let [x1 (first @left)
            y1 (last @left)
            x2 (first @right)
            y2 (last @right)]
        (+ (* (/ (- raw x1) (- x2 x1)) (- y2 y1)) y1)))))

(defn get-curve-points []
  (if-let [col (try
                 (map #(config % :items) (config (select @curve-frame [:#curve]) :items))
                 (catch Exception e
                   nil))]
        (let [res (atom {})]
    (doseq [[_ r a c] col]
      (if (= "->" (config a :text))
        (let [rval (Float/parseFloat (config r :text))
              cval (Float/parseFloat (config c :text))]
          (swap! res assoc rval cval))))
    (into (sorted-map) @res))))

;; conversion in progress...
(defn put-curve-points [curve]
  (if-let [col (try
                 (map #(config % :items) (config (select @curve-frame [:#curve]) :items))
                 (catch Exception e
                   nil))]
    (doseq [[b r a c] col]
      (if (= "" (config a :text)) (.doClick b))
      (let [rval (Float/parseFloat (config r :text))
            cval (interpolate curve rval)]
        (config! c :text (str cval)))
      )))
          

(defn update-curve-scores []
  (let [curvecol (select @curve-frame [:#curve-scores])
        rawcol (select @curve-frame [:#raw-scores])
        rawnums (map #(config % :text) (config rawcol :items))
        curvenums (map #(interpolate (get-curve-points) (Float/parseFloat %)) rawnums)
        curvepane (map #(mk-txt %) curvenums)]
    (config! curvecol :items curvepane)))

(defn enter-update [e]
  (let [k (.getKeyChar e)]
    (if (= k \newline)
      (update-curve-scores))))

(defn mk-curve-button [x c]
  (let [xtxt (format "%.1f" (float x))
        ctxt (format "%.1f" (float c))
        raw (text :text ""
                  :size [80 :by 30]
                  :listen [:key-pressed enter-update]
                  :halign :center)
        arrow (label :text ""
                     :size [40 :by 30]
                     :halign :center)
        crv (text :text ""
                  :size [80 :by 30]
                  :listen [:key-pressed enter-update]
                  :halign :center)
        but (button :text "-"
                    ;;:editable? false
                    :listen [:action
                             (fn [e]
                               (config! arrow :text (if (= ""   (config arrow :text)) "->" ""  ))
                               (config! raw   :text (if (= ""   (config arrow :text)) ""   xtxt))
                               (config! crv   :text (if (= ""   (config arrow :text)) ""   ctxt))
                               (update-curve-scores)
                               )]
                    :size [40 :by 15]
                    :halign :center)
        ]
    (horizontal-panel
     :items [but raw arrow crv]
     )))

(defn mk-histpane [hist]
  (let [svec (atom [])
        nvec (atom [])
        histpane (select @curve-frame [:#histpane])]
    (doseq [[s n] (into (sorted-map) hist)]
      (swap! svec conj s)
      (swap! nvec conj n))
    (let [percs (mk-percentiles @nvec)
          percpane (map #(mk-txt (format "%.1f" %)) percs)
          spane (map #(mk-txt %) @svec)
          npane (map #(mk-txt %) @nvec)
          cnums (map #(interpolate (get-curve-points) %) @svec)
          cpane (map #(mk-txt %) cnums)
          butnums (map #(/ (+ %1 %2) 2) (rest @svec) (drop-last @svec))
          assigntotal (last @svec)
          butnumsextra (postjoin (conj butnums 0) assigntotal)
          butpane (map #(mk-curve-button % (* 100 (/ % assigntotal))) butnumsextra)]
      (config! histpane :items
               [(vertical-panel :items (reverse spane)
                                :id :raw-scores)
                (vertical-panel :items (reverse npane))
                (vertical-panel :items (reverse percpane))
                (vertical-panel :items (reverse butpane)
                                :id :curve)
                (vertical-panel :items (reverse cpane)
                                :id :curve-scores)]))))

(defn choose-assignment-tocurve [e]
  ;;(println "handler")
  (let [assign (selection (-> e .getSource))
        scorestr (get-scores assign)
        scores (parse-to-number-list scorestr)
        hist (mk-histogram scores)]
    (mk-histpane hist)))

(defn mk-curve-entry [curve assign]
  (let [colname (str assign " curve")
        newdict (atom @basedict)]
    (doseq [[k h] @basedict]
      (let [raw (h assign)
            rawnum (try (Float/parseFloat raw)
                        (catch Exception e (str "caught exception: " (.getMessage e))))
            crv (if (number? rawnum)
                  (interpolate curve rawnum)
                  raw)]
        (swap! newdict assoc k (assoc h colname (str crv)))))
    (swap! cols postjoin colname)
    (reset! basedict @newdict)
    (update-text)))

(defn save-curve [e]
  (let [savefile (choose-file :type :save
                              ;;:filters ["Text" ["txt"]]
                              )
        clobber (atom false)]
    (if (.exists  savefile)
      (let [dial (dialog :content (str "File " savefile " exists, do you want to overwrite it?")
                         :success-fn (fn [e] (reset! clobber true))
                         :type :warning
                         :option-type :yes-no)]
        (pack! dial)
        (show! dial))
      (reset! clobber true))
    (if (and savefile clobber)
      (spit savefile (get-curve-points)))))

(defn load-curve [targ]
  (let [loadfile (choose-file :type :open
                              :selection-mode :files-only)]
    (when loadfile
      (let [curve (load-string (slurp loadfile))]
        (put-curve-points curve)))))

(defn save-hist 
  "Save histogram of current assignment"
  [e]
  (let [chooser (select @curve-frame [:#assignment-tocurve])
        assign (selection chooser)
        scorestr (get-scores assign)
        scores (parse-to-number-list scorestr)
        hist (mk-histogram scores)
        filled (fill-hist hist)
        csv (hist->csv filled)]
    (save-csv csv)))
    
(defn mk-curve-frame []
  (let [chooser (combobox :id :assignment-tocurve
                          :model @cols
                          :size [200 :by 30]
                          :listen [:selection (fn [e] (choose-assignment-tocurve e))])
        doit (fn [e]
               (mk-curve-entry (get-curve-points)(selection chooser))
               )]
    (frame :title "Curve"
           :size [600 :by 800]
           :content (horizontal-panel
                     :items
                     [(vertical-panel
                       :items [(horizontal-panel
                                :items [chooser 
                                        (button :text "Apply"
                                                :listen [:action doit]
                                                :size [80 :by 30]
                                                :halign :center)
                                        (label :text " "
                                               :size [40 :by 30]
                                               :halign :center)
                                        (button :text "Save Curve"
                                                :listen [:action save-curve]
                                                :size [120 :by 30]
                                                :halign :center)
                                        (button :text "Load Curve"
                                                :listen [:action load-curve]
                                                :size [120 :by 30]
                                                :halign :center)
                                        (label :text " "
                                               :size [40 :by 30]
                                               :halign :center)
                                        (button :text "Save Hist."
                                                :listen [:action save-hist]
                                                :size [120 :by 30]
                                                :halign :center)]
)
                               (scrollable (horizontal-panel
                                            :id :histpane
                                            :items []))])]))))

(def vertical-break 10)

(def control-frame
  (frame :title "CSV Wrangler"
         :content (horizontal-panel
                   :items
                   [(vertical-panel
                     :items [;; load file
                             (button :id :load-file-base
                                     :text "Load base file"
                                     :listen [:action (fn [e] (load-csv "base"))]
                                     :size [200 :by 30]
                                     :halign :center)
                             (button :id :load-file-add
                                     :text "Load add file"
                                     :listen [:action (fn [e] (load-csv "add"))]
                                     :size [200 :by 30]
                                     :halign :center)
                             (label :text " "
                                   :size [200 :by vertical-break]
                                   :halign :center)
                             (button :id :merge-col
                                     :text "Merge Columns"
                                     :listen [:action (fn [e] (merge-dicts "col"))]
                                     :size [200 :by 30]
                                     :halign :center)
                             (button :id :merge-row
                                     :text "Merge Rows"
                                     :listen [:action (fn [e] (merge-dicts "row"))]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; Load Answer key
                             (label :text " "
                                    :size [200 :by vertical-break]
                                    :halign :center)
                             (button :id :load-key
                                     :text "Load answer key"
                                     :listen [:action (fn [e] (read-key))]
                                     :size [200 :by 30]
                                     :halign :center)
                             (button :id :gradebutt
                                     :text "Grade Exam"
                                     :listen [:action (fn [e] (grade-exam-init))]
                                     :size [200 :by 30]
                                     :halign :center)
                             (label :text " "
                                    :size [200 :by vertical-break]
                                    :halign :center)
                             (button :id :gradecurvebutt
                                     :text "Curve Assign."
                                     :listen [:action (fn [e]
                                                        (reset! curve-frame (mk-curve-frame))
                                                        (-> @curve-frame show!))]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; save merged (w canvas format)
                             (label :text " "
                                    :size [200 :by vertical-break]
                                    :halign :center)
                             (button :id :save-merged
                                     :text "Save Canvas CSV"
                                     :listen [:action
                                              (fn [e]
                                                (save-csv (write-csv (dict->canvas @basedict)))) ]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; save remark file
                             (button :id :save-rmk
                                     :text "Save Remark CSV"
                                     :listen [:action
                                              (fn [e]
                                                (save-csv (write-csv (rmk->canvas @adddict)))) ]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; save LOs as csv
                             (button :id :clear-all-data
                                     :text "Save LOs CSV"
                                     :listen [:action
                                              (fn [e]
                                                (save-csv (write-csv (hash->csv los-hash)))) ]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; clear data
                             (label :text " "
                                    :size [200 :by vertical-break]
                                    :halign :center)
                             (button :id :save-los
                                     :text "Clear All Data"
                                     :listen [:action
                                              (fn [e]
                                                (clear-all-data)) ]
                                     :size [200 :by 30]
                                     :halign :center)
                             ])
                    (vertical-panel
                     :items [;;show basecsv
                             (text :id :base-text-label
                                   :text "Base csv"
                                   :size [100 :by 30]
                                   :halign :center)
                             (scrollable (text :id :base-text
                                               :text ""
                                               :multi-line? true
                                               :wrap-lines? false)
                                         :size [800 :by 300])
                             ;;show ww/rmk csv
                             (text :id :add-text-label
                                   :text "Add csv"
                                   :size [100 :by 30]
                                   :halign :center)
                             (scrollable (text :id :add-text
                                               :text ""
                                               :multi-line? true
                                               :wrap-lines? false
                                               :font (seesaw.font/font :name :monospaced))
                                         :size [800 :by 300])
                             ;;show answer key
                             (text :id :key-text-label
                                   :text "answer key"
                                   :size [100 :by 30]
                                   :halign :center)
                             (scrollable (text :id :key-text
                                               :text ""
                                               :multi-line? true
                                               :wrap-lines? false
                                               :font (seesaw.font/font :name :monospaced))
                                         :size [800 :by 100])
                             ])
                    ])))

;; Uncomment before building!!!
(config! control-frame :on-close :exit)

(defn -main
  "GUI"
  [& args]
  (pack! control-frame)
  (show! control-frame))

;; (defn -main
;;   "ask for files to merge"
;;   [& args]
;;   (while (and @load? (not (and (or @addcsv @rmkcsv) @basecsv)))
;;     (load-csv))
;;   (when @load?
;;     (reset! cols (get-cnvcols))
;;     (make-colsmap)
;;     (make-wwdict)
;;     (make-cnvdict)
;;     (merge-dicts "col")
;;     (save-csv (write-csv (dict->canvas @basedict))))
;;   )
