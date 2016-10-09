(defproject csv-merge "0.1.0-SNAPSHOT"
  :description "merge Canvas csv with Webwork csv"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [seesaw "1.4.5"]]
  ;;  :plugins [[cider/cider-nrepl "0.14.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :main ^:skip-aot csv-merge.core)
