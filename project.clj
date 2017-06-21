(defproject entity/entity-core "0.1.0-SNAPSHOT"
  :description "A library to define domain types"
  :url "https://github.com/inqwell/entity-core"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [entity/entity-sql "0.1.1-SNAPSHOT"]
                 [typeops "0.1.1"]]
  :plugins [[lein-codox "0.10.3"]]
  :profiles
  {:dev
                   {:dependencies [[com.h2database/h2 "1.4.195"]]}
   :resource-paths ["test/resources"]})
