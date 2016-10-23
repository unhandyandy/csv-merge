(ns csv-merge.core
  (:gen-class)
  (:use clojure-csv.core
        seesaw.core
        seesaw.chooser))

(native!)

(def wwcsv (atom nil))
(def cnvcsv (atom nil))
(def wwdict (atom {}))
(def cnvdict (atom {}))
(def cols (atom '()))
(def colsmap (atom {}))
(def drop-cols 6)
(def load? (atom true))

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
        
(defn make-wwdict []
  (reset! wwdict (tab->mapmap (drop 1 @wwcsv) 1 drop-cols)))

(defn make-cnvdict []
  (reset! cnvdict (tab->mapmap @cnvcsv 2 0)))

(defn get-wwcols []
  (drop drop-cols (second @wwcsv)))

(defn get-cnvcols []
  (first @cnvcsv))

(defn find-first-match [item coll]
  (first (drop-while #(not (ww-col-match item %)) coll)))

(defn make-colsmap []
  (let [wwcols (get-wwcols)
        cnvcols (get-cnvcols)]
    (reset! colsmap {})
    (doseq [c wwcols]
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

(defn load-csv []
  (let [loadfile (choose-file :type :open
                              :selection-mode :files-only
                              ;; :dir (-> (java.io.File. ".") .getCanonicalPath)
                              :cancel-fn (fn [e] (reset! load? false)))]
    (if loadfile
      (let [csvstr (slurp loadfile)
            csvtab (parse-csv csvstr)]
        (if (check-ww csvtab)
          (reset! wwcsv csvtab)
          (if (check-cnv csvtab)
            (reset! cnvcsv csvtab)
            (println "Unknown type of csv file.")))))))

(defn merge-dicts []
  (doseq [[id row] @wwdict]
    (merge-row row id)))

(defn dict->canvas [dict]
  (let [ids (sort-by #((@cnvdict %) "Student") (keys dict))
        [_ [test :as status]] @cnvcsv]
    (loop [tab (if (= test "")
                 (list status @cols)
                 (list @cols))
           [id & idrem] ids]
      (let [newrow (loop [row '()
                          [col & colrem] @cols]
                     (if colrem
                       (recur (conj row (or ((dict id) col) "")) colrem)
                       (vec (reverse row))))
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


(defn -main
  "ask for files to merge"
  [& args]
  (while (and @load? (not (and @wwcsv @cnvcsv)))
    (load-csv))
  (when @load?
    (reset! cols (get-cnvcols))
    (make-colsmap)
    (make-wwdict)
    (make-cnvdict)
    (merge-dicts)
    (save-csv (write-csv (dict->canvas @cnvdict))))
  )
