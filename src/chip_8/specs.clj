(ns chip-8.specs
  (:require [clojure.java.io :as io]))

;;; This namespace identifies and implements the specification of the chip-8
;;; language, aiming for a more robust interpreter.

(def memory-size 4096)

(def screen-width  64)
(def screen-height 32)

(def program-memory-pos
  "Indicates the initial position where programs should be loaded into memory."
  0x200)

(def sprites
  "These are the only sprites loaded automatically into chip-8 memory.
  They represent the hexadecimal digits from 0 to F and must be loaded in
  the memory from 0x000 to 0x1FF."
  [0xF0 0x90 0x90 0x90 0xF0   ; 0
   0x20 0x60 0x20 0x20 0x70   ; 1
   0xF0 0x10 0xF0 0x80 0xF0   ; 2
   0xF0 0x10 0xF0 0x10 0xF0   ; 3
   0x90 0x90 0xF0 0x10 0x10   ; 4
   0xF0 0x80 0xF0 0x10 0xF0   ; 5
   0xF0 0x80 0xF0 0x90 0xF0   ; 6
   0xF0 0x10 0x20 0x40 0x40   ; 7
   0xF0 0x90 0xF0 0x90 0xF0   ; 8
   0xF0 0x90 0xF0 0x10 0xF0   ; 9
   0xF0 0x90 0xF0 0x90 0x90   ; A
   0xE0 0x90 0xE0 0x90 0xE0   ; B
   0xF0 0x80 0x80 0x80 0xF0   ; C
   0xE0 0x90 0x90 0x90 0xE0   ; D
   0xF0 0x80 0xF0 0x80 0xF0   ; E
   0xF0 0x80 0xF0 0x80 0x80]) ; F

(def keyboard-mapping
  "This defines a equivalence between the original chip-8 keys and the mappings
  needed in a modern keyboard to not break your fingers.
  The key represents the mapping and the value represents the ascii of the
  original key."
  {:1 0x1 :2 0x2 :3 0x3 :4 0xC
   :q 0x4 :w 0x5 :e 0x6 :r 0xD
   :a 0x7 :s 0x8 :d 0x9 :f 0xE
   :z 0xA :x 0x0 :c 0xB :v 0xF})

(def init-memory
  "Sets the state of the memory at boot time."
  (vec (concat sprites
               (repeat (- memory-size (count sprites)) 0))))

(def init-screen
  "Sets the state of the screen at boot time."
  (vec (repeat (* screen-width screen-height) 0)))

(defn system
  "Definition of the chip-8 machine with all of it's components."
  []
  ;; As stated, chip-8 had 16 general-purpose 8-bit registers. These went
  ;; from 0 to F, prefixed with an V. Register VF shouldn't be used by
  ;; any program, since it's used as a flag register by some instructions.

  ;; I is a 16-bit register used mostly to store memory addresses.

  ;; DT and ST are special purpose registers that act as delay and sound
  ;; timers, respectively.
  {:reg {:v0 0 :v1 0 :v2 0 :v3 0 :v4 0 :v5 0
         :v6 0 :v7 0 :v8 0 :v9 0 :va 0 :vb 0
         :vc 0 :vd 0 :ve 0 :vf 0 :dt 0 :st 0
         :i  0}
   ;; PC is a 16-bit pseudo-register, not accessible from the user program,
   ;; that stores the current executing address.
   :pc program-memory-pos
   ;; SP is a 8-bit pseudo-register, not accessible from the user program,
   ;; that stores the topmost level of the stack.

   ;; In this interpreter, -1 indicates a nil value, since nothing is in the
   ;; stack.
   :sp -1
   ;; The stack is an array of 16 16-bit values, and it's used to store the
   ;; address a program should return after finishing a subroutine. Since
   ;; it's an array of 16 positions, only 16 levels of nested subroutines
   ;; are allowed.
   :stk []
   ;; The memory consists of an array of 4096 bytes (4KB), which served to
   ;; store both the original interpreter and the user program. The interpreter
   ;; was stored from 0x000 to 0x1FF, and should not be used by any program.

   ;; User programs, as state above, were loaded starting at 0x200, but in
   ;; a later model (ETI 660), programs were loaded at 0x600.
   :mem init-memory
   ;; In the original implementation, chip-8 had a 64x32 pixels monochrome
   ;; display.
   :scr init-screen
   ;; Key isn't part of the chip-8 specification, but it acts as a kind of
   ;; register that stores the value of the key that is being pressed.
   ;; This allows an easier implementation of the keyboard peripheral.
   :key nil
   ;; Another element that isn't part of the specification, but allows to
   ;; keep track of when the screen should be redrawed.
   :draw-event false})

(defn get-file-contents
  "Read the contents of a file as a ByteArray."
  [file]
  (with-open [in  (io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    ;; convert from byte to int
    (map #(bit-and % 0xFF) (.toByteArray out))))

(defn load-rom
  "Load the contents of the rom into memory."
  [mem file]
  (let [content   (vec (get-file-contents file))
        final-pos (+ program-memory-pos (count content))]
    (apply assoc mem
           (interleave (range program-memory-pos final-pos)
                       content))))
