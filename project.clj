(defproject net.jeffhui/check.statem "1.0.0-SNAPSHOT"
  :description "Provides ability to test using a state machine as a specification"
  :url "https://github.com/jeffh/check.statem"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/test.check "0.10.0"]]
  :plugins [[lein-codox "0.10.7"]]
  :codox {:metadata   {:doc/format :markdown}
          :source-uri "https://github.com/jeffh/check.statem/blob/{version}/{filepath}#L{line}"})
