(defproject language-extensions "1.3.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]]
  :source-paths ["src/language_extensions"]
  :java-source-paths ["src/language_extensions/java_source"]
  :aot :all
  :main clojure-source.core)
