#lang sicp



;루트
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (square x) (* x x))

(define (root guess x)
  (cond
    ((good-enough? guess x) guess)
    (else (root (improve guess x) x))))

(define (sqrt4 x)
  (root 1.0 x))

;팩토리얼 recursive
(define (factorial n)
  (cond
    ((= n 0) 1)
    (else (* n (factorial (- n 1))))))
;팩토리얼 iteration
(define (iter-fact product counter max-count)
  (cond
    ((> counter max-count) product)
    (else (iter-fact (* product counter)
                     (+ counter 1)
                     max-count))))
;팩토리얼 내부정의
(define (factorial-inner n)
  (define (iter-fact product counter)
    (if (> counter n)
        product
        (iter-fact (* product counter) (+ counter 1))))
  (iter-fact 1 1))

;피보나치 recursive
(define (fibo n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fibo (- n 1))
             (fibo (- n 2))))))
;피보나치 iteration
(define (fib-iter a b count)
  (cond
    ((= count 0) b)
    (else (fib-iter (+ a b) a (- count 1)))))

;피보나치 내부정의


;거듭제곱 fast
(define (exp-fast b n)
  (cond
    ((= n 0) 1)
    ((even? n) (square (exp-fast b (/ n 2))))
    (else (* b (exp-fast b (- n 1))))))

;GCD
(define (gcd-sicp a b)
  (cond
    ((= b 0) a)
    (else (gcd-sicp b (remainder a b)))))

;expmod
(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

  