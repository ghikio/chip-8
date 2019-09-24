(ns chip-8.screen
  (:require [chip-8.specs :as specs]
            [quil.core :as q]
            [quil.middleware :as qm]))

(def scale-factor  16)
(def scaled-width  (* specs/screen-width  scale-factor))
(def scaled-height (* specs/screen-height scale-factor))

(defn get-pos
  ([x y]
   (get-pos x y 1))
  ([x y f]
   (+ (/ x f) (* specs/screen-width (/ y f)))))

(defn screen-nth
  "Return the screen pixel at coordinate (x,y). If f is specified, it will act as the scale factor."
  ([scr x y]
   (screen-nth scr x y 1))
  ([scr x y f]
   (nth scr (get-pos x y f))))
