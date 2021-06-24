#lang sicp

;유리수 연산
;덧셈
(define (add-rat x y)
  (let ((nx (numer x))
        (dx (denom x))
        (ny (numer y))
        (dy (denom y)))
    (make-rat (+ (* nx dy) (* ny dx)) (* dx dy))))
;뺄셈
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
;곱셈
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
;나눗셈
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
;동치
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;유리수 만들기
(define (make-rat n d)
  (cons n d))

;분자
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

;분모
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;분수 출력
(define (print-rat x)
;(newline) ; Racket에서 작업하는 한 필요 없음
  (display (numer x))
  (display "/")
  (display (denom x)))

;1/2
(define one-half (make-rat 1 2))
;1/3
(define one-third (make-rat 1 3))

;zero
(define zero (lambda (f) (lambda (x) x)))

;list[n] 리스트핥기
(define (list-reff l n)
  (cond
    ((= n 0) (car l))
    (else (list-reff (cdr l) (- n 1)))))

;list.length 리스트 길이
(define (lengthh l)
  (cond
    ((null? l) 0)
    (else (+ 1 (lengthh (cdr l))))))

;list.append 리스트 핥으면서 붙이기
(define (appendd list1 list2)
  (cond
    ((null? list1) list2)
    (else (cons (car list1) (append (cdr list1) list2)))))

;list.last-pair 마지막 원소 1개짜리 리스트
(define (last-pairr l)
  (cond
    ((null? (cdr l)) l)
    (else (last-pairr (cdr l)))))

;last-pair 에러 처리
(define (last-pair x)
  (define (unsafe-iter l)
    (cond ((null? (cdr l)) l)
          (else (unsafe-iter (cdr l)))))
  (cond ((not (list? x)) (error "Not a list:" x))
        ((null? x) (error "Empty list!"))
        (else (unsafe-iter x))))

;list.reverse
(define (reversee l)
  (cond
    ((null? l) nil)
    (else (append (reversee (cdr l)) (list (car l))))))

;list.average
(define (list-avg l)
  (/ (apply + l) (lengthh l)))
;list.average-dt
(define (average-dt . l)
  (/ (apply + l) (lengthh l)))

(define (compose* l)
    (cond
      ((= (length l) 0) identity)
      (else (compo2 (list-ref l 0) (compose* (cdr l))))))


;from-to f t . s
(define (from-to f t . s)
  (define (inter a b k)
    (cond ((> a b) nil)
          (else (cons a (inter (+ a k) b k)))))
  (cond ((> (length s) 1) (error "Too many arguments" (cdr s)))
        (else (inter f t (if (null? s) 1 (car s))))))

;scale-list
(define (scale-list1 items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list1 (cdr items) factor))))

;list 프로시저 적용
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;map scale
(define (scale-list2 items factor)
  (map (lambda (x) (* x factor)) items))

;홀 수 번째 list 반환
(define (odd l)
  (cond
    ((null? (cdr l)) l)
    (else (append (list (car l)) (odd (cdr (cdr l)))))))

;x 보다 작은 list 반환
(define (small-list l x)
  (cond
    ((null? l) l)
    (else (if (< (car l) x)
              (cons (car l) (small-list (cdr l) x))
              (small-list (cdr l) x)))))

;count-leaves
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;scale-tree
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;scale-tree-map
(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;square-tree
(define (square-tree tree)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (* tree tree))
    (else (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

;square-tree-map
(define (square-tree-map t)
  (map (lambda (sub-t) (if (pair? sub-t)
                           (square-tree-map sub-t)
                           (* sub-t sub-t)))
       t))

;tree-map 프로시저
(define (tree-map proc t)
  (cond ((null? t) nil)
        ((pair? t) (cons (tree-map proc (car t))
                         (tree-map proc (cdr t))))
        (else (proc t))))

(define (square x) (* x x))

;트리에 square 프로시저 적용
(define (square-tree2 tree) (tree-map square tree))



;;mid
(define five 5)
(define (triple x) (* 3 x))



(define ident (lambda (x) x))

(define (replicate n x)
  (cond
    ((= n 0) nil)
    (else (cons x (replicate (- n 1) x)))))

(cons 1
      (cons (cons 2 (cons 3 nil))
            (cons (cons 4 (cons 5 nil))
                  nil)))