(ns csv-merge.core
  (:gen-class)
  (:use clojure-csv.core
        seesaw.core
        seesaw.chooser
        ))

(native!)

(def wwcsv (atom nil))
(def cnvcsv (atom nil))
(def rmkcsv (atom nil))
(def wwdict (atom {}))
(def cnvdict (atom {}))
(def rmkdict (atom {}))
(def cols (atom '()))
(def colsmap (atom {}))
(def drop-cols 6)
(def load? (atom true))
(def keytab (atom []))
(def los-list nil)
(def los-hash {})
(def key-ver-pos nil)
(def partial-credit? false)

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

(defn mk-line-readable [v n]
  (let [nameletters (subvec v 0 8)
        codedigits (subvec v 8 14)
        sectiondigits (subvec v 14 19)
        uname (clojure.string/trim (clojure.string/lower-case (clojure.string/join nameletters)))
        code (clojure.string/join (map str codedigits))
        section (clojure.string/join (map str sectiondigits))]
    (vec (concat [uname code section] (subvec v 19)))))
        
(defn mk-readable [tab]
  (let [numq (- (count (first tab)) 19)
        qseq (map #(clojure.string/join (list "EQ" (str %))) (range 1 (inc numq)))
        header (concat ["username" "examcode" "section"] qseq)]
    (concat [(vec header)] (map #(mk-line-readable % numq) tab))))
        

(defn ww-col-match [wwcol cnvcol]
  (let [lenww (count wwcol)
        lencnv (count cnvcol)]
    (if (> lenww lencnv) false
      (let [cnvtrunc (subs cnvcol 0 lenww)]
        (= wwcol cnvtrunc)))))

(defn tab->mapmap [[cols & rows] idindex start]
  (let [dict (atom {})]
    (doseq [r rows]
      (let [rowdict (atom {})
            id (nth r idindex)
            twist (map #(list %1 %2) cols r)]
        (doseq [[c s] (drop start twist)]
          (swap! rowdict assoc c s))
        (swap! dict assoc id @rowdict)))
    @dict))
        
(defn make-rmkdict []
  (let [tabmod @rmkcsv]
    (reset! rmkdict (tab->mapmap tabmod 0 1))))

(defn make-wwdict []
  ;; check if wwcsv is defined, otherwise we must be merging from rmk
  (if @wwcsv
    (reset! wwdict (tab->mapmap (drop 1 @wwcsv) 1 drop-cols))
    (make-rmkdict)))

(defn make-cnvdict []
  (reset! cnvdict (tab->mapmap @cnvcsv 2 0)))

(defn get-rmkcols []
  (drop 1 (first @rmkcsv)))

(defn get-wwcols []
  (drop drop-cols (second @wwcsv)))

(defn get-cnvcols []
  (first @cnvcsv))

(defn find-first-match [item coll]
  (first (drop-while #(not (ww-col-match item %)) coll)))

(defn make-colsmap []
  (let [newcols (concat (get-wwcols) (get-rmkcols))
        cnvcols (get-cnvcols)]
    (reset! colsmap {})
    (doseq [c newcols]
      (let [m (find-first-match c cnvcols)]
        (if m
          (swap! colsmap assoc c m)
          (swap! cols conj c))))))

(defn merge-row [row id]
  (when (@cnvdict id)
    (let [olddict (atom (@cnvdict id))]
      (doseq [[k v] row]
        (let [ck (@colsmap k)
              newk (if ck ck k)]
          (swap! olddict assoc newk v)))
      (swap! cnvdict assoc id @olddict))))

(def control-frame)

(defn update-text []
  (let [cnvfd (select control-frame [:#cnv-text])
        tomergefd (select control-frame [:#tomerge-text])
        keyfd (select control-frame [:#key-text])
        tomergetxt (if @wwcsv @wwcsv @rmkcsv)]
    (text! cnvfd (write-csv @cnvcsv))
    (text! tomergefd (write-csv tomergetxt))
    (text! keyfd (write-csv @keytab))))

(defn load-csv []
  (let [loadfile (choose-file :type :open
                              :selection-mode :files-only
                              ;; :dir (-> (java.io.File. ".") .getCanonicalPath)
                              :cancel-fn (fn [e] (reset! load? false)))]
    (when loadfile
      (let [csvstr (slurp loadfile)
            csvtab (parse-csv csvstr)]
        (if (check-ww csvtab)
          (reset! wwcsv csvtab)
          (if (check-cnv csvtab)
            (reset! cnvcsv csvtab)
            (if (check-remark csvtab)
              (reset! rmkcsv (mk-readable csvtab))
              (println "Unknown type of csv file."))))
        (make-cnvdict)
        (make-wwdict)
        (reset! cols (get-cnvcols))
        (make-colsmap)
        (update-text)
        ))))
(def dict->canvas)

(defn merge-dicts []
  (make-colsmap)
  (let [dict (if (> 0 (count @wwdict)) @wwdict @rmkdict)]
    (doseq [[id row] dict]
      (merge-row row id))
    (reset! cnvcsv (dict->canvas @cnvdict))
    (update-text)))

(defn dict->canvas [dict]
  (let [ids (sort-by #((@cnvdict %) "Student") (keys dict))
        [_ [test :as status]] @cnvcsv]
    (loop [tab (if (= test "")
                 (list status @cols)
                 (list @cols))
           [id & idrem] ids]
      (let [newrow (loop [row '()
                          [col & colrem] @cols]
                     (let [row+  (conj row (or ((dict id) col) ""))]
                       (if colrem
                         (recur row+ colrem)
                         (vec (reverse row+)))))
            newtab (conj tab newrow)]
        (if idrem
          (recur newtab idrem)
          (reverse newtab))))))

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
      (let [keystr (slurp keyfile)]
        (reset! keytab (vec (parse-csv keystr))))
      (update-text))))

(defn lookup-in-key [v q]
  (get-in @keytab [v q]))

(defn grade-one [v ansvec & {:keys [items] :or {items (range 1 (inc (count ansvec)))}}]
  (let [correct (filter #(= (lookup-in-key v %) (get ansvec (dec %))) items)
        nc (count correct)
        correctlo (filter #(some #{%} correct) los-list) 
        nclo (count correctlo)]
    (when los-list
      (def los-hash (update los-hash nclo inc))
      (doseq [q los-list]
        (when (some #{q} correct)
          (def los-hash (update los-hash (str "Q" q) inc)))))
    nc))

(defn grade-exam [codepos & {:keys [items]
                             :or {items (range 1 (- (count (first @rmkcsv)) 2))}}]
  (if (> (count @keytab) 0)
    (let [headers (conj (first @rmkcsv) "Exam Score")
          newtab (atom [])]
      (when los-list
        (doseq [q los-list]
          (def los-hash (assoc los-hash (str "Q" q) 0)))
        (doseq [n (range (inc (count los-list)))]
          (def los-hash (assoc los-hash n 0)))
        (def los-hash (assoc los-hash "total" (dec (count @rmkcsv)))))
      (doseq [[_ c _ & ans :as row] (rest @rmkcsv)]
        (let [v (read-string (subs c codepos (inc codepos)))
              ansvec (vec ans)
              s (grade-one v ansvec :items items)
              newrow (conj row (str s))]
          (swap! newtab conj newrow)))
      (reset! rmkcsv (concat [headers] @newtab))
      (make-rmkdict)
      (update-text))
    (alert "No answer key loaded."
           :type :error)))

(defn grade-exam-init []
  (let [numq (- (count (first @rmkcsv)) 3)
        qseq (range 1 (inc numq))
        codepos (if key-ver-pos key-ver-pos
                    (input "Examcode position, 0-based: " :choices [0 1 2 3 4 5]))
        los (if los-list los-list
                (input "Learning Outcomes?"))]
    (when (not los-list)
      (def los-list (if (> (count los) 0)
                      (map read-string (clojure.string/split los #","))
                      [])))
    (when (not key-ver-pos)
      (def key-ver-pos codepos))
    (grade-exam codepos)))
        
(defn hash->csv [hash]
  (map (fn [[k v]] [(str k) (str v)]) los-hash))

(defn parse-multi-str [ans]
  (let [len (count ans)]
    (if (= len 1)
      [ans]
      (let [trimmed (subs ans (- len 1))]
        (clojure.string/split trimmed #",")))))

(def control-frame
  (frame :title "CSV Wrangler"
         :content (horizontal-panel
                   :items
                   [(vertical-panel
                     :items [;; load file
                             (button :id :load-file
                                     :text "Load csv file"
                                     :listen [:action (fn [e] (load-csv))]
                                     :size [200 :by 30]
                                     :halign :center)
                             (button :id :merge
                                     :text "Merge CSVs"
                                     :listen [:action (fn [e] (merge-dicts))]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; Load Answer key
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
                             ;; save merged (w canvas format)
                             (button :id :save-merged
                                     :text "Save Canvas CSV"
                                     :listen [:action
                                              (fn [e]
                                                (save-csv (write-csv (dict->canvas @cnvdict)))) ]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; save remark file
                             (button :id :save-rmk
                                     :text "Save Remark CSV"
                                     :listen [:action
                                              (fn [e]
                                                (save-csv (write-csv (dict->canvas @rmkdict)))) ]
                                     :size [200 :by 30]
                                     :halign :center)
                             ;; save LOs as csv
                             (button :id :save-los
                                     :text "Save LOs CSV"
                                     :listen [:action
                                              (fn [e]
                                                (save-csv (write-csv (hash->csv los-hash)))) ]
                                     :size [200 :by 30]
                                     :halign :center)
                             ])
                    (vertical-panel
                     :items [;;show cnvcsv
                             (text :id :cnv-text-label
                                   :text "Canvas csv"
                                   :size [100 :by 30]
                                   :halign :center)
                             (scrollable (text :id :cnv-text
                                               :text ""
                                               :multi-line? true
                                               :wrap-lines? false)
                                         :size [800 :by 300])
                             ;;show ww/rmk csv
                             (text :id :tomerge-text-label
                                   :text "csv to merge"
                                   :size [100 :by 30]
                                   :halign :center)
                             (scrollable (text :id :tomerge-text
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

(defn -main
  "GUI"
  [& args]
  (pack! control-frame)
  (show! control-frame))

;; (defn -main
;;   "ask for files to merge"
;;   [& args]
;;   (while (and @load? (not (and (or @wwcsv @rmkcsv) @cnvcsv)))
;;     (load-csv))
;;   (when @load?
;;     (reset! cols (get-cnvcols))
;;     (make-colsmap)
;;     (make-wwdict)
;;     (make-cnvdict)
;;     (merge-dicts)
;;     (save-csv (write-csv (dict->canvas @cnvdict))))
;;   )
