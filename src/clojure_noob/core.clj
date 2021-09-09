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

(def good? (fn [guess x]
             (<= (- (improve guess x) guess) 0.0001)))

(def average (fn [x y] (/ (+ x y) 2)))

(def improve (fn [guess x]
               (average guess (/ x guess))))

(def sqrt-iter (fn [guess x]
                 (if (good-enough? guess x)
                   guess
                   (sqrt-iter (improve guess x) x))))

(def sqrt (fn [x] (sqrt-iter 1.0 x)))
