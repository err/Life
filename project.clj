(defproject life "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [org.clojars.automata/rosado.processing "1.1.0"]]
  :main life.core
  :jvm-opts ["-Xms256m" "-Xmx1g"])