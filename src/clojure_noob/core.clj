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
;==============1.35====================
(defn fixed-point [f first-guess]
  (def tolerance 0.00001)
  (def close-enough? (fn [v1 v2] (< (abs (- v1 v2)) tolerance)))
  (def try! (fn [guess]
             (let [next (f guess)]
               (if (close-enough? guess next)
                 next
                 (try! next)))))
   (try! first-guess))

(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)
;==========1.37=========
(defn cont-frac [n d k]
  (let [next (fn next [step]
               (if (< step k)
                 (/ (n step) (+ (d step) (next (+ step 1))))
                 0))]
    (next 1)))

(/ 1 (cont-frac (fn [x] 1.0) (fn [x] 1.0) 14))

(defn cont-frac-iter [n d k]
  (let [next (fn next [step acc]
               (if (= step k)
                 acc
                 (next (+ step 1) (/ (n step) (+ (d step) acc)))))]
    (next 0 (/ (n 1) (d 1)))))

(/ 1 (cont-frac-iter (fn [_] 1.0) (fn [_] 1.0) 14))
;========1.40=======================
(def dx 0.00001)

(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))

(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn cubic [a b c]
  (fn [x] (+ (cube x) (* a (square x) (* b x) c))))

(newtons-method (cubic 5 3 1) 1.0)
;=======1.41===================
(defn double [g]
  (fn [x] (g (g x))))

((double inc) 1)
(((double (double double)) inc) 5)
;=======1.42==============
(defn compose [f g]
  (fn [x] (f (g x))))

((compose square inc) 6)
;=======1.43=============
(defn repeated [g n]
  (let [iter (fn iter [n acc]
               (if (= n 1)
                 acc
                 (iter (- n 1) (compose acc g))))]
    (iter n g)))

((repeated square 1) 5)
((repeated square 2) 5)
((repeated square 3) 5)
((repeated square 4) 5)
;;;===========================
;========2.2==============
(defn x-point [point] (first point))
(defn y-point [point] (first (rest point)))

(defn make-point [x y] (list x y))

(defn make-segment [p1 p2] (list p1 p2))

(defn start-segment [seg] (first seg))
(defn end-segment [seg] (first (rest seg)))

(defn midpoint-segment [seg] 
  (let [x1 (x-point (start-segment seg))
        x2 (x-point (end-segment seg))
        y1 (y-point (start-segment seg))
        y2 (y-point (end-segment seg))]
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))


(midpoint-segment (list '(1 0) '(0 1)))
;=========2.3===========================
(defn make-rect [x1 y1 x2 y2]
  (let [p1 (make-point x1 y1)
        p2 (make-point x1 y2)
        p3 (make-point x2 y2)
        p4 (make-point x2 y1)
        seg1 (make-segment p1 p4)
        seg2 (make-segment p1 p2)]
    (list seg1 seg2)))
;========2.4===========
(defn cons [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))
                       
(defn cdr [z]
  (z (fn [p q] q)))

(car (cons 1 3))
(cdr (cons 'a 'b))

;====================2.6==================
(defn zero [f] (fn [x] x))

(defn add-1 [n] (fn [f] (fn [x] (f ((n f) x)))))

(defn one [] (fn [f] (fn [x] (f x))))
;===============2.7======================
(defn make-interval [a b] (list a b))

(defn lower-bound [interval] (min (first interval) (first (rest interval))))

(defn upper-bound [interval] (max (first interval) (first (rest interval))))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y] 
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(defn div-interval [x y] 
  (when (and (< (lower-bound y) 0) (> (upper-bound y) 0))
    (println "Error interval")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(defn sub-interval [a b]
  (make-interval (- (upper-bound a) (lower-bound b))
                 (- (lower-bound a) (upper-bound b))))

;=====================2.12==============
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))


(defn make-center-percent [center percent]
  (let [delta (* center (/ percent 100))]
    (make-interval (+ center delta) (- center delta))))
;============2.17============
(defn last-pair [l]
  (defn last-pair-iter [l acc]
    (if (empty? l)
      acc
      (last-pair-iter (rest l) (first l))))
  (last-pair-iter l (first l)))

(last-pair (list 23 72 149 34))
;==========2.18===============
(defn reverse-cust [l]
  (defn reverse-cust-iter [l acc]
    (if (empty? l)
      (flatten acc)
      (reverse-cust-iter (rest l) (list (first l) acc))))
  (reverse-cust-iter (rest l) (first l)))

(reverse-cust (list 1 4 9 16 25))

;===================================
(defn map-cust [proc items]
  (if (empty? items)
    nil
    (conj (map proc (rest items)) (proc (first items)))))

(map-cust (fn [x] (* x x)) (list 1 2 3 4))

;----------------------------------
(defn count-leaves [x]
  (cond (empty? x) 0
        (not (list? x)) 1
        :default (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

(count-leaves (list 1 (list 2 3)))
