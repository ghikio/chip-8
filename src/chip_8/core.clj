(ns chip-8.core
  (:require [chip-8.screen :as scr]
            [chip-8.specs :as specs]
            [chip-8.instructions :as ins]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(defn get-file-contents
  "Read the contents of a file as a ByteArray."
  [file]
  (with-open [in  (io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    ;; convert from byte to int
    (map #(bit-and % 0xFF) (.toByteArray out))))

(defn load-rom
  "Load the contents of a file into memory."
  [mem file]
  (let [content   (vec (get-file-contents file))
        final-pos (+ specs/program-memory-pos (count content))]
    (apply assoc mem
           (interleave (range specs/program-memory-pos final-pos)
                       content))))

(defn call-ins
  "Calls the instruction `f` with the given parameters `sys` and `args`.
  If `update-pc` is true, program counter is incremented."
  [sys f update-pc & args]
  (as-> (eval `(~f ~sys ~@args)) e
    (if update-pc (ins/inc-pc e) e)
    (ins/dec-timers e)))

(defn evaluate
  "Evaluate an instruction."
  [sys]
  (let [s               (assoc sys :draw-event false) ; dismiss old draw-event
        op              (read-string (ins/get-next-ins s))
        op-match        (vec (format "%04X" op))
        [_ op1 op2 op3] op-match
        call            (partial call-ins s)]
    (match op-match
           [\0 \0 \E \0] (call ins/op-00e0 true)
           [\0 \0 \E \E] (call ins/op-00ee false)
           [\0  _  _  _] (ins/inc-pc sys) ; does nothing
           [\1  _  _  _] (call ins/op-1nnn false (bit-and op 0xFFF))
           [\2  _  _  _] (call ins/op-2nnn false (bit-and op 0xFFF))
           [\3  _  _  _] (call ins/op-3xkk false op1 (bit-and op 0xFF))
           [\4  _  _  _] (call ins/op-4xkk false op1 (bit-and op 0xFF))
           [\5  _  _ \0] (call ins/op-5xy0 false op1 op2)
           [\6  _  _  _] (call ins/op-6xkk true  op1 (bit-and op 0xFF))
           [\7  _  _  _] (call ins/op-7xkk true  op1 (bit-and op 0xFF))
           [\8  _  _ \0] (call ins/op-8xy0 true  op1 op2)
           [\8  _  _ \1] (call ins/op-8xy1 true  op1 op2)
           [\8  _  _ \2] (call ins/op-8xy2 true  op1 op2)
           [\8  _  _ \3] (call ins/op-8xy3 true  op1 op2)
           [\8  _  _ \4] (call ins/op-8xy4 true  op1 op2)
           [\8  _  _ \5] (call ins/op-8xy5 true  op1 op2)
           [\8  _  _ \6] (call ins/op-8xy6 true  op1)
           [\8  _  _ \7] (call ins/op-8xy7 true  op1 op2)
           [\8  _  _ \E] (call ins/op-8xye true  op1)
           [\9  _  _ \0] (call ins/op-9xy0 false op1 op2)
           [\A  _  _  _] (call ins/op-annn true  (bit-and op 0xFFF))
           [\B  _  _  _] (call ins/op-bnnn false (bit-and op 0xFFF))
           [\C  _  _  _] (call ins/op-cxkk true  op1 (bit-and op 0xFF))
           [\D  _  _  _] (call ins/op-dxyn true  op1 op2 (bit-and op 0xF))
           [\E  _ \9 \E] (call ins/op-ex9e false op1)
           [\E  _ \A \1] (call ins/op-exa1 false op1)
           [\F  _ \0 \7] (call ins/op-fx07 true  op1)
           [\F  _ \0 \A] (call ins/op-fx0a true  op1)
           [\F  _ \1 \5] (call ins/op-fx15 true  op1)
           [\F  _ \1 \8] (call ins/op-fx18 true  op1)
           [\F  _ \1 \E] (call ins/op-fx1e true  op1)
           [\F  _ \2 \9] (call ins/op-fx29 true  op1)
           [\F  _ \3 \3] (call ins/op-fx33 true  op1)
           [\F  _ \5 \5] (call ins/op-fx55 true  op1)
           [\F  _ \6 \5] (call ins/op-fx65 true  op1)
           :else (.throw (Exception. (str "opcode '" op "' not found"))))))
