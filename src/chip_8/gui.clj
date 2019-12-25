(ns chip-8.gui
  (:require [chip-8.screen :as scr]
            [chip-8.specs :as specs]
            [chip-8.instructions :as ins]
            [quil.core :as q]
            [quil.middleware :as qm]))

;;; This namespace implements all the logic related with the ui, allowing
;;; for more flexibility in the future if implementing another one.

;; I couldn't find a way to pass the file without using mutation,
;; since setup can't accept parameters and it's the one that
;; starts the system.
(def rom-file (atom ""))

(defmacro until [timeout & body]
  `(let [prom#  (promise)
         out#   (promise)
         work#  (future (deliver prom# ~@body))
         timer# (future (do (Thread/sleep ~timeout) (deliver out#)))
         res#   (loop []
                  (cond (realized? prom#) (deref prom#)
                        (realized? out#)  :timeout
                        :else (do (Thread/sleep 1)
                                  (recur))))]
     (do (future-cancel work#)
         (future-cancel timer#)
         res#)))

(defn setup
  []
  (q/frame-rate 60)
  (q/no-stroke)
  (let [sys (specs/system)]
    (assoc sys :mem (specs/load-rom (:mem sys) @rom-file))))

(defn update-state
  [state]
  (if-not (state :closed?)
    (let [interim (atom state)
          res     (until 16
                         (loop [acc (ins/evaluate state)]
                           (reset! interim acc)
                           (if (:draw-event acc)
                             acc
                             (recur (ins/evaluate acc)))))]
      (case res
        :timeout @interim
        res))
    state))


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
    :on-close     (fn [state] (assoc state :closed? true))
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
