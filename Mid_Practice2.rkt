#lang sicp

;a-b 더하는 추상 프로시저
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;세제곱
(define (cube x) (* x x x))
;세제곱 합
(define (cube-sum a b)
  (sum cube a inc b))
;합
(define (int-sum a b)
  (sum identity a inc b))
;교대 급수
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;인테그랄
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

;람다 교대 급수
(define (pi-sum-l a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))
;람다 인테그랄
(define (integral-l f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))
;람다 응용
(define (square x) (* x x))
(define plus4 (lambda (x) (+ x 4)))
;((lambda (x y z) (+ x y (square z))) 1 2 3) == 12
;((lambda (x) (* 5 2) (+ 42 x 56)) 100) -> (+ 42 100 56)
;((lambda (x) (define (foo t) (* t 2)) (+ x (foo x))) 5) == 15

;f(x,y)
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a)) ; f-helper는 자기 바깥의
       (* y b) ; x, y를 뻔히 보고 있음
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

;f(x,y)-lam
(define (f-lam x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;f(x,y)-let == f-lam
(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))


;이분법 근 찾기 
(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))
;방향
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))
;(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0) == 1.89 ..

;fn(x) recursive
;(define (iter-app f n x)
;  (cond ((> n 0) (f (iter-app f (- n 1) x)))
;        ((= n 0) x)))

;fn(x) iterative 함수반복
(define (iter-app f n x)
  (cond ((> n 0) (iter-app f (- n 1) (f x)))
        ((= n 0) x)))

(define (iter-app1 f n)
  (lambda (x)
  (cond ((> n 0) (iter-app f (- n 1) (f x)))
        ((= n 0) x))))

;(iter-app (lambda (x) (* 2 x)) 10 1) == 1024
;(iter-app double 3.2 1) == void

;함수 반복 display
(define (double x) (* x 2))
(define (iter-app-disp f n x)
  (display x) (newline)
  (cond ((> n 0) (iter-app-disp f (- n 1) (f x)))))
;(iter-app-disp double 10 1) == 1 2 4 8 16 .. 1024

;고정점 찾기
(define (fixpoint f x)
  (let ((fx (f x)))
    (cond ((= x fx) x)
          (else (fixpoint f fx)))))
;고정점 display
(define (fixpoint-disp f x)
  (let ((fx (f x)))
    (display x) (newline)
    (cond ((not (= x fx)) (fixpoint-disp f fx)))))

;고정 사이클
(define (cycle-example x)
  (cond ((even? x) (/ x 2))
        (else (- x 3))))
;(iter-app-disp cycle-example 14 61) == 61 58 .. -2 -1 -4 -2 -1 -4

;고정점 추정치
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;제곱근 추정치
(define (sqrt-guess x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;프로시저를 리턴 프로시저


; 첫번째 방식 (내부 정의)
(define (compo1 f g)
  (define (foo x)
    (f (g x)))
foo)
; 두번째 방식 (람다식을 리턴)
(define (compo2 f g)
  (lambda (x) (f (g x))))

(define (compose* l)
    (cond
      ((= (length l) 0) identity)
      (else (compo2 (list-ref l 0) (compose* (cdr l))))))

(define (triple x)
  (display 3)
  (display "*")
  (display x)
  (display "=")
  (* 3 x))


; 세번째 방식 (프로시저도 람다식)
(define compo3
  (lambda (f g)
    (lambda (x) (f (g x)))))
; 네번째 방식 (요건 몰랐지?)
(define ((compo4 f g) x)
  (f (g x)))

;((compo1 square double) 10) == 400
;((compo2 double inc) 5) == 12

(define (compo f g)
  (lambda (x) (g (f x))))