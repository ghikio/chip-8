(ns chip-8.gui
  (:require [chip-8.screen :as scr]
            [chip-8.core :as core]
            [chip-8.specs :as specs]
            [quil.core :as q]
            [quil.middleware :as qm]))

;;; This namespace implements all the logic related with the ui, allowing
;;; for more flexibility in the future if implementing another one.

(defn setup
  []
  (q/frame-rate 30)
  (q/no-stroke)
  (let [sys (specs/system)]
    (assoc sys :mem (core/load-rom (:mem sys) "/home/ghikio/src/chip-8/roms/INVADERS"))))

(defn update-state
  [state]
  (prn "dt: " (get-in state [:reg :dt]))
  (prn "st: " (get-in state [:reg :dt]))
  (prn "f:  " (get-in state [:reg :vf]))
  (core/evaluate state))

(defn draw-state
  [state]
  (when (:draw-event state)
    (q/background 0)
    (q/fill 245)
    (doseq [y (range 0 (q/height) scr/scale-factor)
            x (range 0 (q/width) scr/scale-factor)]
      (when (pos? (scr/screen-nth (:scr state) x y scr/scale-factor))
        (q/rect x y scr/scale-factor scr/scale-factor)))))

(defn start
  []
  (q/defsketch app
    :title        "chip 8"
    :size         [scr/scaled-width scr/scaled-height]
    :setup        setup
    :draw         draw-state
    :update       update-state
    :key-pressed  (fn [state {:keys [key key-code]}]
                    (assoc state :key key))
    :key-released (fn [state _]
                    (assoc state :key nil))
    :middleware   [qm/fun-mode]))
