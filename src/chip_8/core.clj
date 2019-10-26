(ns chip-8.core
  (:require [chip-8.gui :as gui])
  (:gen-class))

(defn -main
  "Starts everything."
  [& args]
  (if (= 1 (count args))
    (do (reset! gui/rom-file (first args))
        (gui/start))
    (.println *err* "usage: ./program <rom-file>")))
