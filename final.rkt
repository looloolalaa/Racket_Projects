#lang sicp
;제곱
(define (square n) (* n n))

;피보나치
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;0 부터 10
(define zero-to-ten (list 0 1 2 3 4 5 6 7 8 9 10))

;map: 일괄적용
;(define (map proc items)
;  (if (null? items)
;      nil
;      (cons (proc (car items))
;            (map proc (cdr items)))))

;필터
(define (filter pred? l)
  (cond ((null? l) nil)
        ((pred? (car l)) (cons (car l) (filter pred? (cdr l))))
        (else (filter pred? (cdr l)))))

;취합: 꼬리재귀 X 시간복잡도: O(n) 공간복잡도: O(n)
(define (accumulate op base l)
  (if (null? l)
      base
      (op (car l) (accumulate op base (cdr l)))))

;지수 취합
(define (expt-var . l)
  (if (null? l)
      1
      (expt (car l) (apply expt-var (cdr l)))))

(define (expt-list l)
  (accumulate expt 1 l))

;왼쪽부터 취합: 꼬리재귀 시간복잡도: O(n) 공간복잡도: O(1)
(define (fold-left op base l)
  (if (null? l)
      base
      (fold-left op (op base (car l)) (cdr l))))

;fold-left 내부정의 
(define (fold-left2 op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;열거 
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;트리 열거 
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))     ;리프
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;트리 홀수 제곱합
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

;짝수 피보나치 리스트
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;트리 홀수 제곱합 재정의
(define (sum-odd-squares2 tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

;짝수 피보나치 리스트 재정의
(define (even-fibs2 n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

;피보나치수 제곱
(define (list-fib-squares n)
  (map square (map fib (enumerate-interval 0 n))))

;홀수제곱들의 곱
(define (product-of-squares-of-odd-elements l)
  (apply * (map square (filter odd? l))))

;프로그래머 최대 급여
;(define (salary-of-highest-paid-programmer records)
;  (accumulate max
;              0
;              (map salary
;                   (filter programmer? records))))


;map 재정의
;(define (map proc l)
;  (accumulate (lambda (x y) (cons (proc x) y)) nil l))

;append 재정의
;(define (append l1 l2)
;  (accumulate cons l2 l1))

;length 재정의
;(define (length l)
;  (accumulate (lambda (x y) (+ 1 y)) 0 l))

;length 재정의2
;(define (length l)
;  (accumulate + 0 (map (lambda (e) 1) l)))


;팩토리얼
(define (factorial n)
  (accumulate * 1 (enumerate-interval 1 n)))

;replicate
(define (replicate n x)
  (map (lambda (e) x) (enumerate-interval 1 n)))

;프로시저 합성
;(define (compose* . fs)
;  (accumulate compose identity fs))

;iterated
;(define (iterated f n)
;  (accumulate compose identity (replicate n f)))

;퀵 정렬
(define (quick-sort l)
  (cond ((null? l) nil)
        (else (append (quick-sort (filter (lambda (e) (<= e (car l))) (cdr l)))
                      (list (car l))
                      (quick-sort (filter (lambda (e) (> e (car l))) (cdr l)))))))

;소수 리스트
;(define (primes-upto n)
;  (filter prime? (enumerate-interval 2 n)))

;소수 리스트2
(define (primes-upto n)
  (define (not-divided-by d)
    (lambda (i) (not (= (remainder i d) 0))))
  (define (sieve l)
    (cond ((null? l) nil)
          (else (cons (car l)
                      (sieve (filter (not-divided-by (car l)) (cdr l)))))))
  (sieve (enumerate-interval 2 n)))

;all
(define ((all pred?) l)
  (or (null? l)
      (and (pred? (car l))
           ((all pred?) (cdr l)))))

;any
(define ((any pred?) l)
  (and (not (null? l))
       (or (pred? (car l))
           ((any pred?) (cdr l)))))

;andmap
(define ((andmap pred?) l . ls)
  ((all (lambda (l) (apply pred? l)))
   (apply map list l ls))) ;가로세로 뒤집 

;ormap
(define ((ormap pred?) l . ls)
  ((any (lambda (l) (apply pred? l)))
   (apply map list l ls))) ;가로세로 뒤집

;f n x
(define (iter-app f n x)
  (cond ((> n 0) (iter-app f (- n 1) (f x)))
        ((= n 0) x)
        (else (error "negative count:" n))))

;f n x 리스트 
(define (iter-app-list#1 f n x)
  (cond ((> n 0) (append (iter-app-list#1 f (- n 1) x) (list (iter-app f n x))))
        ((= n 0) (list x))))

;f n x 리스트2
(define (iter-app-list#2 f n x)
  (map (lambda (k) (iter-app f k x)) (enumerate-interval 0 n)))

;f n x 리스트3
(define (iter-app-list#3 f n x)
  (cond ((> n 0) (cons x (map f (iter-app-list#3 f (- n 1) x))))
        ((= n 0) (list x))))

;f n x 리스트4
(define (iter-app-list#4 f n x)
  (cond ((> n 0) (cons x (iter-app-list#4 f (- n 1) (f x))))
        ((= n 0) (list x))))

;flatmap
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;합이 소수인 순서쌍
;(define (prime-sum-pairs n)
;  (filter prime-sum?
;          (flatmap (lambda (i)
;                     (map (lambda (j) (list i j))
;                          (enumerate-interval 1 (- i 1))))
;                   (enumerate-interval 1 n))))

;합 표시
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;합 표시
;(define (prime-sum-pairs n)
;  (map make-pair-sum
;       (filter prime-sum?
;               (flatmap (lambda (i)
;                          (map (lambda (j) (list i j))
;                               (enumerate-interval 1 (- i 1))))
;                        (enumerate-interval 1 n)))))

;차집합
(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

;순열
(define (permutation s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutation (remove x s))))
               s)))
      
              

