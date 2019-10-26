(ns chip-8.instructions
  (:require [chip-8.screen :as scr]
            [chip-8.specs :as specs]
            [clojure.pprint :as pp]
            [clojure.core.match :refer [match]]))

;; Helpers

(defn num->bin
  "Transforms a number `n` to it's binary form."
  [^Integer n]
  (map (fn [^Character c] (Character/digit c 2)) (pp/cl-format nil "~8,'0',B" n)))

(defn hex-char->num
  "Transforms a character `c` in hex form to an int.
  e.g. E --> 14"
  [^Character c]
  (let [n (int c)]
    (cond
      (>= 0x39 n 0x30) (- n 0x30 #_0)
      (>= 0x46 n 0x41) (+ 10 (- n 0x41 #_A))
      :else (throw (Exception. (str "Can't convert '" c "' to number."))))))

(defn compare-sprite
  "Compare if any bit existing in the old sprite has dissapeared from the new sprite."
  [old new]
  (if-not (seq old)
    true
    (if (pos? (first old))
      (if (= (first old) (first new))
        (recur (rest old) (rest new))
        false)
      (recur (rest old) (rest new)))))

(defn get-mem-sprite
  "Get a sprite from memory."
  [sys ^Integer pos]
  (num->bin (nth (:mem sys) pos)))

(defn get-scr-sprite
  "Get a sprite from the screen."
  [sys ^Integer pos]
  (take 8 (drop pos (:scr sys))))

(defn inc-pc
  "Increase the program counter.
  If `num-ins` is supplied, increases it's value in instructions."
  ([sys]
   (inc-pc sys 1))
  ([sys ^Integer num-ins]
   (update sys :pc + (* 2 num-ins))))

(defn ignore-next-if
  "Ignore next instruction if f on x and y returns true"
  [sys f ^Character x ^Character y]
  (if (f x y)
    (inc-pc sys 2)
    (inc-pc sys)))

(defn get-next-ins
  "Return the instruction pointed by the program counter."
  [sys]
  (when (< (:pc sys) 4096)
    (format "0x%02X%02X"
            (nth (:mem sys) (:pc sys))
            (nth (:mem sys) (+ 1 (:pc sys))))))

(defn get-register
  "Get the content of the given register `x`.
  Parameter `x` represent either a value from 0 to f for general-purpose
  registers or i, d or s for the equivalent specific registers i, dt and st."
  [sys ^Character x]
  (get-in sys [:reg (keyword (str \v x))]))

(defn get-dt [sys] (get-in sys [:reg :dt]))
(defn get-st [sys] (get-in sys [:reg :dt]))
(defn get-i  [sys] (get-in sys [:reg :i]))

(defn set-register
  "Set the content of the given register `x`.
  Parameter `x` represent either a value from 0 to f for general-purpose
  registers or i, d or s for the equivalent specific registers i, dt and st."
  [sys ^Character x ^Integer value]
  (let [v (bit-and (unchecked-byte value) 0xFF)]
    (assoc-in sys [:reg (keyword (str \v x))] v)))

(defn set-dt [sys ^Integer v] (assoc-in sys [:reg :dt] v))
(defn set-st [sys ^Integer v] (assoc-in sys [:reg :st] v))
(defn set-i  [sys ^Integer v] (assoc-in sys [:reg :i]  v))

(defn write-sprite
  "Write a sprite to the screen at pos, being a sprite a list of 8 bits."
  [sys sprite ^Integer pos]
  (assoc sys :scr (apply assoc
                         (:scr sys)
                         (interleave (range pos (+ pos 8)) sprite))))

(defn set-sprite
  [sys old-sprite new-sprite ^Integer pos]
  (let [xored-sprite (map bit-xor old-sprite new-sprite)
        s            (write-sprite sys xored-sprite pos)]
    (if-not (compare-sprite old-sprite xored-sprite)
      (set-register s \f 1)
      s)))

(defn dec-timers
  [sys]
  (let [dt (get-dt sys)
        st (get-st sys)]
    (as-> sys s
      (if-not (<= dt 0) (set-dt s (- dt 2)) (set-dt s 0))
      (if-not (<= st 0) (set-st s (- st 2)) (set-dt s 0)))))

;; Instructions implementation

(defn op-00e0
  "CLS - Clear the display"
  [sys]
  (assoc sys :scr specs/init-screen))

(defn op-00ee
  "RET - Sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer."
  [sys]
  (let [next-pc (last (:stk sys))]
    (if (> (:sp sys) -1)
      (-> sys
          (assoc  :pc  (+ 2 next-pc))
          (update :stk pop)
          (update :sp  dec))
      (inc-pc sys)))) ;; if the stack is empty ignore instruction?

(defn op-1nnn
  "JP addr - sets the program counter to nnn."
  [sys addr]
  (assoc sys :pc addr))

(defn op-2nnn
  "CALL addr - Increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to nnn."
  [sys addr]
  (-> sys
      (update :sp  inc)
      (update :stk conj (:pc sys))
      (assoc  :pc  addr)))

(defn op-3xkk
  "SE Vx, byte - Compares register Vx to kk, and if they are equal, increments the program counter by 2."
  [sys x b]
  (ignore-next-if sys = (get-register sys x) b))

(defn op-4xkk
  "SNE Vx, byte - Compares register Vx to kk, and if they are not equal, increments the program counter by 2."
  [sys x b]
  (ignore-next-if sys not= (get-register sys x) b))

(defn op-5xy0
  "SE Vx, Vy - Compares register Vx to register Vy, and if they are equal, increments the program counter by 2."
  [sys x y]
  (ignore-next-if sys = (get-register sys x) (get-register sys y)))

(defn op-6xkk
  "LD Vx, byte - Puts the value kk into register Vx."
  [sys x b]
  (set-register sys x b))

(defn op-7xkk
  "ADD Vx, byte - Adds the value kk to the value of register Vx, then stores the result in Vx."
  [sys x b]
  (set-register sys x (+ b (get-register sys x))))

(defn op-8xy0
  "LD Vx, Vy - Stores the value of register Vy in register Vx."
  [sys x y]
  (set-register sys x (get-register sys y)))

(defn op-8xy1
  "OR Vx, Vy - Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx."
  [sys x y]
  (set-register sys x (bit-or (get-register sys x)
                              (get-register sys y))))

(defn op-8xy2
  "AND Vx, Vy - Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx."
  [sys x y]
  (set-register sys x (bit-and (get-register sys x)
                               (get-register sys y))))

(defn op-8xy3
  "XOR Vx, Vy - Performs a bitwise XOR on the values of Vx and Vy, then stores the result in Vx."
  [sys x y]
  (set-register sys x (bit-xor (get-register sys x)
                               (get-register sys y))))

(defn op-8xy4
  "ADD Vx, Vy - The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx."
  [sys x y]
  (let [res (+ (get-register sys x) (get-register sys y))]
    (-> sys
        (set-register x  (bit-and res 0xFF))
        (set-register \f (if (> res 255) 1 0)))))

(defn op-8xy5
  "SUB Vx, Vy - If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx."
  [sys x y]
  (let [vx  (get-register sys x)
        vy  (get-register sys y)]
    (-> sys
        (set-register x (- vx vy))
        (set-register \f (if (> vx vy) 1 0)))))

(defn op-8xy6
  "SHR Vx - If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2."
  [sys x]
  (let [vx (get-register sys x)]
    (-> sys
        (set-register x  (quot vx 2))
        (set-register \f (bit-and vx 1)))))

(defn op-8xy7
  "SUBN Vx, Vy - If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx."
  [sys x y]
  (let [vx  (get-register sys x)
        vy  (get-register sys y)]
    (-> sys
        (set-register x (- vy vx))
        (set-register \f (if (> vy vx) 1 0)))))

(defn op-8xye
  "SHL Vx - If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2."
  [sys x]
  (let [vx (get-register sys x)]
    (-> sys
        (set-register x  (* vx 2))
        (set-register \f (bit-and (bit-shift-right vx 7) 1)))))

(defn op-9xy0
  "SNE Vx, Vy - The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2."
  [sys x y]
  (ignore-next-if sys not= (get-register sys x) (get-register sys y)))

(defn op-annn
  "LD I, addr - The value of register I is set to nnn."
  [sys addr]
  (set-i sys addr))

(defn op-bnnn
  "JP V0, addr - The program counter is set to nnn plus the value of V0."
  [sys addr]
  (assoc sys :pc (+ addr (get-register sys \0))))

(defn op-cxkk
  "RND Vx, byte - generates a random number from 0 to 255, which is then ANDed with the value b. The results are stored in Vx."
  [sys x b]
  (set-register sys x (bit-and (rand-int 256) b)))

(defn op-dxyn
  "DRW Vx, Vy, nibble - The interpreter reads n bytes from memory, starting at the address stored in I. These bytes are then displayed as sprites on screen at coordinates (Vx, Vy). Sprites are XORed onto the existing screen. If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0. If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen."
  [sys x y n]
  (let [pos (scr/get-pos (get-register sys x) (get-register sys y))]
    (loop [s (set-register sys \f 0)
           i 0]
      (let [sprite-pos (+ pos (* i 64))
            scr-sprite (get-scr-sprite sys sprite-pos)
            new-sprite (get-mem-sprite sys (+ i (get-i sys)))]
        (if (< i n)
          (recur (set-sprite s scr-sprite new-sprite sprite-pos)
                 (inc i))
          (assoc s :draw-event true))))))

(defn op-ex9e
  [sys x]
  (ignore-next-if sys =
                  (get-register sys x)
                  (get specs/keyboard-mapping (:key sys))))

(defn op-exa1
  [sys x]
  (ignore-next-if sys not=
                  (get-register sys x)
                  (get specs/keyboard-mapping (:key sys))))

(defn op-fx07
  "LD Vx, DT - The value of DT is placed into Vx."
  [sys x]
  (set-register sys x (get-dt sys)))

(defn op-fx0a
  [sys x]
  (prn "NOTICE THIS FUCKING BIG COMMENT")
  sys)

(defn op-fx15
  "LD DT, Vx - DT is set equal to the value of Vx."
  [sys x]
  (set-dt sys (get-register sys x)))

(defn op-fx18
  "LD ST, Vx - ST is set equal to the value of Vx."
  [sys x]
  (set-st sys (get-register sys x)))

(defn op-fx1e
  "ADD I, Vx - The values of I and Vx are added, and the results are stored in I."
  [sys x]
  (set-i sys (+ (get-i sys)
                (get-register sys x))))

(defn op-fx29
  "LD F, Vx - The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx."
  [sys x]
  (set-i sys (* 5 (get-register sys x))))

(defn op-fx33
  "LD B, Vx - Takes the decimal value of Vx, and places the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2."
  [sys x]
  (let [i         (get-i sys)
        vx        (get-register sys x)
        vx-digits [(quot vx 100) (quot (mod vx 100) 10) (mod vx 10)]]
    (-> sys
        (assoc-in [:mem i]       (vx-digits 0))
        (assoc-in [:mem (inc i)] (vx-digits 1))
        (assoc-in [:mem (+ i 2)] (vx-digits 2)))))

(defn op-fx55
  "LD [I], Vx - Copies the values of registers V0 through Vx into memory, starting at the address in I."
  [sys x]
  (let [xnum   (hex-char->num x)
        values (map #(get-register sys %) (range (inc xnum)))]
    (loop [s sys
           k 0]
      (if (<= k xnum)
        (recur (assoc-in s [:mem (+ (get-i s) k)] (values k))
               (inc k))))))

(defn op-fx65
  "LD Vx, [I] - The interpreter reads values from memory starting at location I into registers V0 through Vx."
  [sys x]
  (let [xnum   (hex-char->num x)
        values (take (inc xnum) (drop (get-i sys) (:mem sys)))]
    (loop [s sys
           k 0]
      (if (<= k xnum)
        (recur (set-register s k (nth values k))
               (inc k))
        s))))

(defn call-ins
  "Calls the instruction `f` with the given parameters `sys` and `args`.
  If `update-pc` is true, program counter is incremented."
  [sys f update-pc & args]
  (as-> (eval `(~f ~sys ~@args)) e
    (if update-pc (inc-pc e) e)
    (dec-timers e)))

(defn evaluate
  "Evaluate an instruction."
  [sys]
  (let [s               (assoc sys :draw-event false) ; dismiss old draw-event
        op              (read-string (get-next-ins s))
        op-match        (vec (format "%04X" op))
        [_ op1 op2 op3] op-match
        call            (partial call-ins s)]
    (match op-match
           [\0 \0 \E \0] (call op-00e0 true)
           [\0 \0 \E \E] (call op-00ee false)
           [\0  _  _  _] (inc-pc sys) ; does nothing
           [\1  _  _  _] (call op-1nnn false (bit-and op 0xFFF))
           [\2  _  _  _] (call op-2nnn false (bit-and op 0xFFF))
           [\3  _  _  _] (call op-3xkk false op1 (bit-and op 0xFF))
           [\4  _  _  _] (call op-4xkk false op1 (bit-and op 0xFF))
           [\5  _  _ \0] (call op-5xy0 false op1 op2)
           [\6  _  _  _] (call op-6xkk true  op1 (bit-and op 0xFF))
           [\7  _  _  _] (call op-7xkk true  op1 (bit-and op 0xFF))
           [\8  _  _ \0] (call op-8xy0 true  op1 op2)
           [\8  _  _ \1] (call op-8xy1 true  op1 op2)
           [\8  _  _ \2] (call op-8xy2 true  op1 op2)
           [\8  _  _ \3] (call op-8xy3 true  op1 op2)
           [\8  _  _ \4] (call op-8xy4 true  op1 op2)
           [\8  _  _ \5] (call op-8xy5 true  op1 op2)
           [\8  _  _ \6] (call op-8xy6 true  op1)
           [\8  _  _ \7] (call op-8xy7 true  op1 op2)
           [\8  _  _ \E] (call op-8xye true  op1)
           [\9  _  _ \0] (call op-9xy0 false op1 op2)
           [\A  _  _  _] (call op-annn true  (bit-and op 0xFFF))
           [\B  _  _  _] (call op-bnnn false (bit-and op 0xFFF))
           [\C  _  _  _] (call op-cxkk true  op1 (bit-and op 0xFF))
           [\D  _  _  _] (call op-dxyn true  op1 op2 (bit-and op 0xF))
           [\E  _ \9 \E] (call op-ex9e false op1)
           [\E  _ \A \1] (call op-exa1 false op1)
           [\F  _ \0 \7] (call op-fx07 true  op1)
           [\F  _ \0 \A] (call op-fx0a true  op1)
           [\F  _ \1 \5] (call op-fx15 true  op1)
           [\F  _ \1 \8] (call op-fx18 true  op1)
           [\F  _ \1 \E] (call op-fx1e true  op1)
           [\F  _ \2 \9] (call op-fx29 true  op1)
           [\F  _ \3 \3] (call op-fx33 true  op1)
           [\F  _ \5 \5] (call op-fx55 true  op1)
           [\F  _ \6 \5] (call op-fx65 true  op1)
           :else (throw (Exception. (str "opcode '" op "' not found"))))))
