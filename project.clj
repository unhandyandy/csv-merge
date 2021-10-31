(defproject csv-merge "0.4-SNAPSHOT"
  :description "merge Canvas csv with Webwork csv"
  :url "http://"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [clojure-csv/clojure-csv "2.0.2"]
                 [seesaw "1.4.5"]
                 [org.flatland/ordered "1.15.10"]
                 [table "0.5.0"]
                 ]
  ;; :plugins [[lein-bin "0.3.4"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :main ^:skip-aot csv-merge.core
  :bin {:name "csv-merge"
        :bin-path "~/bin"
        :bootclasspath false})
