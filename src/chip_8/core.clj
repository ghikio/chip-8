(ns chip-8.core
  (:require [chip-8.gui :as gui]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "Starts everything."
  [& args]
  (if (= 1 (count args))
    (do (reset! gui/rom-file (first args))
        (gui/start))
    (.println *err* "usage: ./program <rom-file>")))

(defn list-roms []
  (map #(.getName %) (file-seq (io/file "./ROMS/"))))

;;Helper function to easily load ROMs from the repl...
(defn test-rom [name]
  (do (reset! gui/rom-file (str "./roms/" name))
      (gui/start)))

