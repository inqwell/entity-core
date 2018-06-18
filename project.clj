(defproject entity/entity-core "0.1.1"
  :description "A library to define domain types"
  :url "https://github.com/inqwell/entity-core"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]

                 [typeops "0.1.1"]
                 [com.rpl/specter "1.1.0"]]
  :plugins [[lein-codox "0.10.3"]]
  :codox {:output-path "codox/entity-core"
          :source-uri "https://github.com/inqwell/entity-core/blob/master/{filepath}#L{line}"}
  :profiles {:dev            {:dependencies [[com.h2database/h2 "1.4.195"]
                                             [entity/entity-sql "0.1.2"]]}})
