#lang sicp
(define (sum l)
  (define (accumulate op base l)
  (if (null? l)
      base
      (op (car l) (accumulate op base (cdr l)))))
  (accumulate + 0 l))

(define (sums . l)
  (apply map + l))

(define (prods . l)
  (apply map * l))

(define (accumulate op base l)
  (if (null? l)
      base
      (op (car l) (accumulate op base (cdr l)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

  

(define (matrix-plus l . ls)
  ((lambda (l) (apply map sums l))
   (apply map list l ls)))



(define (transpose l . ls)
  (apply map list l ls))

(define (matrix? l)
  (cond (lambda (x) (number? x)) l))
  
  