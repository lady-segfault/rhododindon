(defproject rhododindon "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-typed "0.3.5"]]
  :dependencies [[org.clojure/clojure "1.7.0-beta3"]
                 [org.clojure/core.typed "0.2.84"]]
  :core.typed {:check [rhododindon.pdf]}
  :main ^:skip-aot rhododindon.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
