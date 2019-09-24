(ns chip-8.instructions-test
  (:require [chip-8.instructions :as ins]
            [clojure.test :refer :all]))

(def sys (chip-8.core/init-system))

(deftest register-test
  (testing "The content of a given register can be accessed and changed"
    (let [s (ins/set-register sys \0 0x20)]
      (is 0x20 (= (ins/get-register s \0))))))

(deftest dt-test
  (testing "The content of dt can be accessed and changed"
    (let [s (ins/set-dt sys 0x20)]
      (is 0x20 (= (ins/get-dt s))))))

(deftest st-test
  (testing "The content of st can be accessed and changed"
    (let [s (ins/set-st sys 0x20)]
      (is (= 0x20 (ins/get-st s))))))

(deftest op-00e0-test
  (testing "Display is cleared"
    (is (= 0
           (count (filter pos? (:scr (ins/op-00e0 sys))))))))

(deftest op-00ee-test
  (testing "If stack is not empty pc = stack[sp] && sp--."
    (let [s (ins/op-00ee (-> sys
                             (update :stk conj 0x20)
                             (update :sp  inc)))]
      (is (= 0x20 (:pc  s)))
      (is (= -1   (:sp  s)))
      (is (empty? (:stk s)))))
  (testing "If the stack is empty, instruction is ignored."
    (let [expected-pc (+ 2 (:pc sys))
          s           (ins/op-00ee sys)]
      (is (= expected-pc (:pc s))))))

(deftest op-1nnn-test
  (testing "pc = addr"
    (let [s (ins/op-1nnn sys 0x20)]
      (is (= 0x20 (:pc s))))))

(deftest op-2nnn-test
  (testing "sp++ && stk[sp] = pc && pc = addr"
    (let [s (ins/op-2nnn sys 0x20)]
      (is (= 0x20  (:pc s)))
      (is (= 0     (:sp s)))
      (is (= 0x200 (last (:stk s)))))))

(deftest op-3xkk-test
  (testing "Vx == kk && pc += 4"
    (let [expected-pc (+ (:pc sys) 4)
          s           (-> sys
                          (assoc-in [:reg :v0] 0x20)
                          (ins/op-3xkk \0 0x20))]
      (is (= expected-pc (:pc s)))))
  (testing "Vx and kk are not equal."
    (let [expected-pc (+ (:pc sys) 2)
          s           (ins/op-3xkk sys \0 0x20)]
      (is (= expected-pc (:pc s))))))

(deftest op-4xkk-test
  (testing "Vx != kk && pc += 4"
    (let [expected-pc (+ (:pc sys) 4)
          s           (ins/op-4xkk sys \0 0x20)]
      (is (= expected-pc (:pc s)))))
  (testing "Vx == kk && pc += 2"
    (let [expected-pc (+ (:pc sys) 2)
          s           (-> sys
                          (assoc-in [:reg :v0] 0x20)
                          (ins/op-4xkk \0 0x20))]
      (is (= expected-pc (:pc s))))))

(deftest op-5xy0-test
  (testing "Vx == Vy && pc += 4"
    (let [expected-pc (+ (:pc sys) 4)
          s           (-> sys
                          (assoc-in [:reg :v0] 0x20)
                          (assoc-in [:reg :v1] 0x20)
                          (ins/op-5xy0 \0 \1))]
      (is (= expected-pc (:pc s)))))
  (testing "Vx /= Vy && pc += 2"
    (let [expected-pc (+ (:pc sys) 2)
          s           (-> sys
                          (assoc-in [:reg :v0] 0x40)
                          (assoc-in [:reg :v1] 0x20)
                          (ins/op-5xy0 \0 \1))]
      (is (= expected-pc (:pc s))))))

(deftest op-6xkk-test
  (testing "Vx = kk"
    (let [s (ins/op-6xkk sys \0 0x20)]
      (is (= 0x20 (ins/get-register s \0))))))

(deftest op-7xkk-test
  (testing "Vx = Vx + kk"
    (let [s (-> sys
                (ins/set-register \0 0x10)
                (ins/op-7xkk \0 0x20))]
      (is (= 0x30 (ins/get-register s \0))))))

(deftest op-8xy0-test
  (testing "Vx = Vy"
    (let [s (-> sys
                (ins/set-register \1 0x20)
                (ins/op-8xy0 \0 \1))]
      (is (= 0x20 (ins/get-register s \0))))))

(deftest op-8xy1-test
  (testing "Vx = Vx xor Vy"
    (let [s (-> sys
                (ins/set-register \0 2r1101)
                (ins/set-register \1 2r1011)
                (ins/op-8xy1 \0 \1))]
      (is (= 2r1111 (ins/get-register s \0))))))

(deftest op-8xy2-test
  (testing "Vx = Vx and Vy"
    (let [s (-> sys
                (ins/set-register \0 2r1101)
                (ins/set-register \1 2r1011)
                (ins/op-8xy2 \0 \1))]
      (is (= 2r1001 (ins/get-register s \0))))))

(deftest op-8xy3-test
  (testing "Vx = Vx xor Vy"
    (let [s (-> sys
                (ins/set-register \0 2r1101)
                (ins/set-register \1 2r1011)
                (ins/op-8xy3 \0 \1))]
      (is (= 2r0110 (ins/get-register s \0))))))

(deftest op-8xy4-test
  (testing "Vx = Vx + Vy && Vf = 1"
    (let [s (-> sys
                (ins/set-register \0 2r10011001) ;; 153
                (ins/set-register \1 2r01111000) ;; 120
                (ins/op-8xy4 \0 \1))]
      (is (= 2r00010001 (ins/get-register s \0)))
      (is (= 1          (ins/get-register s \f)))))
  (testing "Vx = Vx + Vy && Vf = 0"
    (let [s (-> sys
                (ins/set-register \0 2r00011001) ;; 025
                (ins/set-register \1 2r01111000) ;; 120
                (ins/op-8xy4 \0 \1))]
      (is (= 2r10010001 (ins/get-register s \0)))
      (is (= 0          (ins/get-register s \f))))))

(deftest op-8xy5-test
  (testing "Vx = Vx - Vy && Vf = 1"
    (let [s (-> sys
                (ins/set-register \0 120)
                (ins/set-register \1  60)
                (ins/op-8xy5 \0 \1))]
      (is (= 60 (ins/get-register s \0)))
      (is (= 1  (ins/get-register s \f)))))
  (testing "Vx = Vx - Vy && Vf = 0"
    (let [s (-> sys
                (ins/set-register \0  60)
                (ins/set-register \1 120)
                (ins/op-8xy5 \0 \1))]
      (is (= -60 (ins/get-register s \0)))
      (is (= 0   (ins/get-register s \f))))))

(deftest op-8xy6-test
  (testing "Vx = Vx / 2 && Vf = 1"
    (let [s (-> sys
                (ins/set-register \0 2r11000011) ;; 195
                (ins/op-8xy6 \0))]
      (is (= 97 (ins/get-register s \0)))
      (is (= 1  (ins/get-register s \f)))))
  (testing "Vx = Vx / 2 && Vf = 0"
    (let [s (-> sys
                (ins/set-register \0 2r11000010) ;; 194
                (ins/op-8xy6 \0))]
      (is (= 97 (ins/get-register s \0)))
      (is (= 0  (ins/get-register s \f))))))

(deftest op-8xy7-test
  (testing "Vx = Vy - Vx && Vf = 1"
    (let [s (-> sys
                (ins/set-register \0  60)
                (ins/set-register \1 120)
                (ins/op-8xy7 \0 \1))]
      (is (= 60 (ins/get-register s \0)))
      (is (= 1  (ins/get-register s \f)))))
  (testing "Vx = Vy - Vx && Vf = 0"
    (let [s (-> sys
                (ins/set-register \0 120)
                (ins/set-register \1  60)
                (ins/op-8xy7 \0 \1))]
      (is (= -60 (ins/get-register s \0)))
      (is (= 0   (ins/get-register s \f))))))

(deftest op-8xye-test
  (testing "Vx = Vx * 2 && Vf = 1"
    (let [s (-> sys
                (ins/set-register \0 2r10000000) ;; 128
                (ins/op-8xye \0))]
      (is (= 256 (ins/get-register s \0)))
      (is (= 1   (ins/get-register s \f)))))
  (testing "Vx = Vx * 2 && Vf = 0"
    (let [s (-> sys
                (ins/set-register \0 2r01000000) ;; 64
                (ins/op-8xye \0))]
      (is (= 128 (ins/get-register s \0)))
      (is (= 0   (ins/get-register s \f))))))

(deftest op-9xy0-test
  (testing "Vx /= Vy && pc += 4"
    (let [expected-pc (+ (:pc sys) 4)
          s           (-> sys
                          (assoc-in [:reg :v0] 0x40)
                          (assoc-in [:reg :v1] 0x20)
                          (ins/op-9xy0 \0 \1))]
      (is (= expected-pc (:pc s)))))
  (testing "Vx == Vy && pc += 2"
    (let [expected-pc (+ (:pc sys) 2)
          s           (-> sys
                          (assoc-in [:reg :v0] 0x20)
                          (assoc-in [:reg :v1] 0x20)
                          (ins/op-9xy0 \0 \1))]
      (is (= expected-pc (:pc s))))))

(deftest op-annn-test
  (testing "I = addr"
    (let [s (ins/op-annn sys 0x20)]
      (= 0x20 (:i s)))))

(deftest op-bnnn-test
  (testing "pc = Vx + addr"
    (let [s (-> sys
                (ins/set-register \0 0x20)
                (ins/op-bnnn 0x10))]
      (is (= 0x30 (:pc s))))))

;; op-cxkk-test -- can't test a random number

(deftest op-dxyn
  (testing "Load of sprite."
    (let [s (-> sys
                (ins/set-register \0 16)
                (ins/set-register \1 0)
                (assoc :i 0x480)
                (assoc :mem (apply assoc
                                   (:mem sys)
                                   (interleave (range 0x480 0x488)
                                               (repeat 8 1))))
                (ins/op-dxyn \0 \1 8))]
      (is (= (take 8 (drop 16 (:scr s)))
             (repeat 8 1)))))
  (testing "Load of a sprite with right collision."
    (let [s (-> sys
                (ins/set-register \0 58)
                (ins/set-register \1 0)
                (assoc :i 0x480)
                (assoc :mem (apply assoc
                                   (:mem sys)
                                   (interleave (range 0x480 0x488)
                                               (repeat 8 1))))
                (ins/op-dxyn \0 \1 8))]
      (is (= (take 8 (drop 58 (:scr s)))
             (repeat 8 1)))))
  (testing "Load of a sprite with erased pixels."
    (let [s (-> sys
                (ins/set-register \0 16)
                (ins/set-register \1 0)
                (assoc :i 0x480)
                (assoc :mem (apply assoc
                                   (:mem sys)
                                   (interleave (range 0x480 0x488)
                                               (repeat 8 1))))
                (assoc :scr (apply assoc
                                   (:scr sys)
                                   (interleave (range 16 24)
                                               '(1 1 0 0 1 0 1 0))))
                (ins/op-dxyn \0 \1 8))]
      (is (= (take 8 (drop 16 (:scr s)))
             '(0 0 1 1 0 1 0 1))))))

(deftest op-fx07-test
  (testing "Vx = DT"
    (let [s (ins/set-dt sys 0x20)]
      (is (= 0x20 (ins/get-register (ins/op-fx07 s \0) \0))))))

(deftest op-fx15-test
  (testing "DT = Vx"
    (let [s (ins/set-register sys \0 0x20)]
      (= 0x20 (ins/get-dt (ins/op-fx15 s \0))))))

(deftest op-fx18-test
  (testing "ST = Vx"
    (let [s (ins/set-register sys \0 0x20)]
      (= 0x20 (ins/get-st (ins/op-fx18 s \0))))))

(deftest op-fx1e-test
  (testing "I = Vx + I"
    (let [s (-> sys
                (assoc :i 0x20)
                (ins/set-register \0 0x20))]
      (is (= 0x40 (:i (ins/op-fx1e s \0)))))))

(deftest op-fx29-test
  (testing "I = hex sprite for value Vx"
    (let [s (-> sys
                (ins/set-register \0 3)
                (ins/op-fx29 \0))]
      (is (= 15 (:i s)))
      (is (= (take 5 (drop (:i s) (:mem s)))
             '(0xF0 0x10 0xF0 0x10 0xF0)))))) ;; 3

(deftest op-fx33-test
  (testing "I = hundreds Vx, I + 1 = tens Vx, I + 2 = ones Vx"
    (let [s (-> sys
                (ins/set-register \0 128)
                (ins/op-fx33 \0))
          i (:i s)
          m (:mem s)]
      (is (= 1 (nth m i)))
      (is (= 2 (nth m (inc i))))
      (is (= 8 (nth m (+ 2 i)))))))

(deftest op-fx55-test
  (testing "I = V0, I + 1 = V1, ..., I + x = Vx"
    (let [s (-> sys
                (assoc :i 0x800)
                (ins/set-register \0 1)
                (ins/set-register \1 1)
                (ins/set-register \2 0)
                (ins/set-register \3 1)
                (ins/op-fx55 3))
          m (:mem s)]
      (is (= 1 (nth m 0x800)))
      (is (= 1 (nth m 0x801)))
      (is (= 0 (nth m 0x802)))
      (is (= 1 (nth m 0x803))))))

(deftest op-fx65-test
  (testing "V0 = mem(I), V1 = mem(I) + 1, ..., Vx = mem(I) + x"
    (let [s (-> sys
                (assoc :i 0x800)
                (assoc :mem (apply assoc
                                   (:mem sys)
                                   (interleave (range 0x800 0x804)
                                               '(3 4 10 7))))
                (ins/op-fx65 3))]
      (is (=  3 (ins/get-register s \0)))
      (is (=  4 (ins/get-register s \1)))
      (is (= 10 (ins/get-register s \2)))
      (is (=  7 (ins/get-register s \3))))))
