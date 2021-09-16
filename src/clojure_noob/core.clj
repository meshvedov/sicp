(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;(def print-rat (fn [x] (printf "%s/%s" (numer x) (denom x))))
(def square (fn [x] (* x x)))
(def sum-square (fn [a b c]
                  (if (> a b)
                    (+ (square (if (> b c) b c)) (square a))
                    (+ (square (if (> a c) a c)) (square b)))))
;=====1.1.7=======
(def abs (fn [x]
           (if (< x 0) (- x) x)))

(def good-enough? (fn [guess x]
                    (< (abs (- (square guess) x)) 0.001)))

(def average (fn [x y] (/ (+ x y) 2)))

(def improve (fn [guess x]
               (average guess (/ x guess))))

(def good? (fn [guess x]
             (<= (- (improve guess x) guess) 0.0001)))

(def sqrt-iter (fn [guess x]
                 (if (good-enough? guess x)
                   guess
                   (sqrt-iter (improve guess x) x))))

(def sqrt (fn [x] (sqrt-iter 1.0 x)))
;=====1.8======
(defn cube [x] (* x x x))

(defn improve-cube [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn good-enough-cube? [guess x]
  (< (abs (- (cube guess) x)) 0.001))

(defn cube-iter [guess x]
  (if (good-enough-cube? guess x)
    guess
    (cube-iter (improve-cube guess x) x)))

(defn cube-sqrt [x] (cube-iter 1.0 x))
;====1.11=====
(defn ff [n]
  (cond (< n 3) n
        (>= n 3) (+ (ff (- n 1)) (ff (- n 2)) (ff (- n 3)))))

(defn f-iter
  ([n] (if (< n 3)
         n
         (f-iter n 3 2 1 0)))
  ([m n fn1 fn2 fn3] (if (= m n)
                       (+ fn1 fn2 fn3)
                       (f-iter m
                               (+ n 1)
                               (+ fn1 fn2 fn3)
                               fn1
                               fn2))))
;======1.16=================
(defn power
  ([b n] (power b n 1))
  ([b n a] (cond (= n 0) a
                 (even? n) (recur (*' b b) (/ n 2) a)
                 :else (recur b (- n 1) (*' a b)))))

;===========================
;=====1.17========
(defn mul
  ([a b] (mul a b 0))
  ([a b acc] (cond (= b 0) acc
                   (even? b) (mul a (/ b 2) (+ acc (* a (/ b 2))))
                   :else  (mul a (- b 1) (+ a acc)))))
(mul 137 17)

(defn mul-fast [a b]
  (cond (= b 0) 0
        (even? b) (* 2 (mul-fast a (/ b 2)))
        :else (+ a (mul-fast a (- b 1)))))
(mul-fast 2 10)
;===========================
;==========1.29=============
(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn sum-iter [term a next b]
  (let [iter (fn [a res] (if (> a b)
                           res
                           (recur (next a) (+ res (term a)))))]
    (iter a 0)))

(defn integral [f a b dx]
  (let [add-dx (fn [x] (+ x dx))]
    (* (sum f (+ a (/ dx 2)) add-dx b)
       dx)))

(integral cube 0 1 0.01)

(defn integral-simps [f a b n]
  (let [h (/ (- b a) n)
        y (fn [k] (* (f (+ a (* k h))) (cond (= k 0) 1
                                             (= k n) 1
                                             (even? k) 2
                                             :else 4)))
        add-n (fn [x] (+ x 1))]
    (* (sum-iter y 0 add-n n)
       (/ h 3))))

(integral-simps cube 0 1 100)
;======1.31==============
(defn product [term a next b]
  (if (= a b)
    (term b)
    (* (term a) (product term (next a) next b))))

(defn fibonaci [n]
  (let [idn (fn [x] x)
        next (fn [x] (+ x 1))]
  (product idn 1 next n)))

(fibonaci 5)

;(defn pi-numer [a b]
;  (let [term (fn [x] (* x x))
;        next (fn [x] (+ x 2))]
;    (product term a next b)))

;(defn pi [n] 
;  (* 4.0 (/ (* (/ 2 n) (pi-numer 4 n)) (pi-numer 3 (- n 1)))))

;(pi 20)
(defn pi2 [precision]
  (let [enumerator (fn [index] (cond (even? index) (+ index 2.0)
                                     :else (+ index 1.0)))
        denominator (fn [index] (cond (even? index) (+ index 1.0)
                                      :else (+ index 2.0)))
        fraction (fn [index] (/ (enumerator index) (denominator index)))]
    (* 4 (product fraction 1 inc precision))))
(pi2 1280)
;==================1.32============
(defn accumulate [combiner null-value term a next b]
  (let [iter (fn [a result] 
               (if (= a b)
                 (combiner (term a) result)
                 (recur (next a) (combiner (term a) result))))]
    (iter a null-value)))

(defn accumulate-iter [combiner null-value term a next b]
  (defn iter [a result]
               (if (= a b)
                 (combiner (term a) result)
                 (combiner (iter (next a) (term a)) result)))
    (iter a null-value))

(defn product32 [term a next b]
  (accumulate-iter * 1 term a next b))
(product32 identity 1 inc 10)

(defn sum32 [term a next b]
  (accumulate-iter + 0 term a next b))
(sum32 identity 0 inc 10)
;========================================
;============1.33=====================
(defn filtered-accumulate [combiner null-value term a next b filter]
  (defn iter [a result]
   (if (= a b)
    (combiner (if (filter a) (term a) null-value) result)
    (iter (next a) (combiner (if (filter a) (term a) null-value) result))))
  (iter a null-value))
;======================================
