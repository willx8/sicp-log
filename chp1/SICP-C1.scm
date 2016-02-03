#lang scheme
(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1)) (* 2 (f1 (- n 2))) (* 3 (f1 (- n 3)))))
        ))
(define (f2 n)
  (if (<= n 2)
      n
      (f-iter 0 1 2 n)))
(define (f-iter a b c count)
  (if (<= count 2)
      c
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (square n) (* n n))

;excercies 1.16
(define (fast-expt-iter b n)
  (expt-iter n b 1))
(define (expt-iter n b a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (/ n 2) (square b) a))
        (else (expt-iter (- n 1) b (* a b)))))

;exercise 1.17&1.18
(define (halv b) (/ b 2))
(define (double a) (* a 2))
;;;recursive version
(define (re* a b)
  (cond ((= b 0) 0)
        ((= b 1) a) 
        ((even? b) (re* (double a) (halv b)))
        (else (+ a (re* a (- b 1))))))
;;;iterative version
(define (it* a b)
  (if (= b 0)
      0
      (it*-iter a b 0)))
(define (it*-iter a b sum)
  (cond ((= b 1) (+ a sum))
        ((even? b) (it*-iter (double a) (halv b) sum))
        (else (it*-iter a (- b 1) (+ sum a)))))

;exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(define (naive-fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;exercise 1.22
(define found? 0)
(define (runtime) (current-milliseconds))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      ""
      ))
(define (report-prime elapsed-time)
  (define found? 1)
  (display " *** ")
  (display elapsed-time))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;search for the all odd primes less than n
(define (search-for-primes n)
  (define (search-for-primes-iter count)
    (timed-prime-test count)
    (if (prime? count)
        (newline)
        (search-for-primes-iter (+ count 2))))
  (if (even? n)
      (search-for-primes-iter (+ n 1))
      (search-for-primes-iter n)))


;1.3
;exercise 1.29
;Simpson's Rule: let h = (b-a)/n, yk = f(a+kh)
;(h/3)*(y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-1 + yn)
(define (cube n) (* n n n))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (inc k)
  (+ k 1))
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (ny k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (/ h 3)
     (sum ny 0 inc n)))

;exercies 1.30 iterative version of sum
(define (isum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;1.31.a
;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;         (product term (next a) next b))))
(define (fact n)
  (define (self a) a)
  (product self 1 inc n))
(define (pi/4 n)
  (define (next a) (+ a 2))
  (define (term a) (* (/ (- a 1.0) a) (/ (+ a 1.0) a)))
  (product term 3 next n))

;1.31.b iterative product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;1.32
;iterative
(define (iaccumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
;using accumulate to compute pi
(define (pi)
  (define (next a) (+ a 2))
  (define (term a) (* (/ (- a 1.0) a) (/ (+ a 1.0) a)))
  (raccumulate * 4 term 3 next 1000000)) ;to use iteration change raccmulate to iaccmulate, vice versa
;recursive
(define (raccumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (raccumulate combiner null-value term (next a) next b))))

;1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (filter a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))
;1.33a
(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))
;1.33
(define (product-of-relative-primes n)
  (define (relative-prime? b)
    (= (gcd b n) 1))
  (define (self a) a)
  (filtered-accumulate * 1 self 1 inc n relative-prime?))
;1.34
(define (f g) (g 2))

;1.35
(define tolerance 0.00000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;1.35 try this:
;(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 0.5)
;1.36 try this:
;(fixed-point (lambda (x) (/ (log 1000) (log x))) 5)
;1.37 iter:
(define (cont-frac n d k)
  (define (cont-frac-iter k result)
    (if (= k 0)
        result
        (cont-frac-iter (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-iter k 0))
;1.37 recur:
(define (r-cont-frac n d k)
  (define (cont-frac-recur n d i)
    (if (= i (+ k 1))
        0
        (/ (n i) (+ (d i) (cont-frac-recur n d (+ i 1))))))
  (cont-frac-recur n d 1))
; try this:
;(cont-frac /* or r-cont-frac */ (lambda (i) 1.0)
;             (lambda (i) 1.0)
;             100)

;1.38 use cont-frac to compute e
(define (e-natural-log-base k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= 2 (remainder i 3))
                      (+ 2 (/ i 3))
                      1))
                k)))

;1.39 use cont-frac to compute tangent
(define (tan-cf x k)
  (cont-frac (lambda (i)
                  (if (= i 1)
                      x
                      (- 0 (square x))))
                (lambda (i) (- (* 2.0 i) 1.0))
                k))