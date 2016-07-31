(defproject fullpath "0.1.0-SNAPSHOT"
  :description "Fullpath program for Clojure"
  :url "http://example.com/FIXME"
  :license {:name "WTFPL"
            :url "http://www.wtfpl.net/about/"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:dependencies [[ring/ring-devel "1.4.0"]]}} ; <-- not sure what this is, or whether I actually need it
  :main fullpath.core
  :aot [fullpath.core])
