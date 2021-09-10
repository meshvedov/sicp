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

(defn fff [n]
  (fff-iter (- n 1) (- n 2) (- n 3) 0 n))

(defn fff-iter [a b c acc counter]
  (if (< a 2)
    acc
    (fff-iter (- a 1) (- b 2) (- c 3) (+ acc
                                         (if (< a 3) a 0)
                                         (if (< b 3) b 0)
                                         (if (< c 3) c 0)) (- counter 1))))

