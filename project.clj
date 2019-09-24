(defproject chip-8 "0.0.1"
  :description "CHIP-8 machine emulator"
  :url "https://github.com/ghikio/chip-8"
  :license {:name "BSD-3-Clause"
            :url "https://spdx.org/licenses/BSD-3-Clause.html"}
  :dependencies [[quil "3.0.0"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "0.3.0"]]
  :jvm-opts ["-Xms4g" "-Xmx4g"]
  :main chip-8.start
  :aot :all
  :repl-options {:init-ns chip-8.start})
