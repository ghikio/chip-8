(ns chip-8.start
  (:require [chip-8.gui :as gui])
  (:gen-class))

(defn -main
  "Starts everything."
  [& args]
  (gui/start))
