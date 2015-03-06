(defproject pe-rest-utils "0.0.2-SNAPSHOT"
  :description "A Clojure library providing a set of helper functions for building REST APIs on top of Datomic."
  :url "https://github.com/evanspa/pe-rest-utils"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}
  :plugins [[lein-pprint "1.1.2"]
            [codox "0.8.10"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/data.codec "0.1.0"]
                 [pe-core-utils "0.0.4"]
                 [pe-datomic-utils "0.0.3"]
                 [ch.qos.logback/logback-classic "1.0.13"]
                 [liberator "0.12.2"]
                 [org.slf4j/slf4j-api "1.7.5"]
                 [com.datomic/datomic-free "0.9.5130"
                  :exclusions [org.slf4j/slf4j-nop
                               joda-time
                               org.slf4j/slf4j-log4j12]]
                 [environ "1.0.0"]]
  :resource-paths ["resources"]
  :codox {:exclude [user]
          :src-dir-uri "https://github.com/evanspa/pe-rest-utils/blob/0.0.1/"
          :src-linenum-anchor-prefix "L"}
  :profiles {:dev {:source-paths ["dev"]
                   :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]
                   :dependencies [[org.clojure/tools.namespace "0.2.7"]
                                  [org.clojure/java.classpath "0.2.2"]
                                  [org.clojure/tools.nrepl "0.2.7"]]}}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :creds :gpg}]])
