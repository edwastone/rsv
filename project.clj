(defproject rsv "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.1.8"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/java.jdbc "0.3.2"]
                 [postgresql "9.1-901.jdbc4"]
                 [org.clojure/data.json "0.2.5"]
                 [clojurewerkz/urly "1.0.0"]
                 [enlive "1.1.1"]
                 [net.cgrand/moustache "1.1.0"]
                 [ring/ring-core "1.2.0"]]
  :plugins [[lein-ring "0.8.11"]]
  :ring {:handler rsv.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})

