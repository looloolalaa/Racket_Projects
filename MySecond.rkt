#lang sicp

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        (else (- x))))

(define (square x) (* x x))

(define (up b n)
  (cond ((= n 0) 1)
        ((even? n) (square (up b (/ n 2))))
        (else (* b (up b (- n 1))))))

(define (GCD x y)
  (cond ((= y 0) x)
        (else (GCD y (remainder x y)))))


; 프로시저 정의
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))


(define (cube x) (* x x x))

