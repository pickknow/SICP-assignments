#lang racket
;union-set  并集
;intersection-set 交集
;element-of-set?
;adjoin-set 加入新集合
(define (element-of-set1? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set1? x (cdr set)))))
(define (adjoin-set1 x set)
  (if (element-of-set1? x set)
      set
      (cons x set)))
(define (intersection-set1 set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set1? (car set1) set2)
         (cons (car set1)
               (intersection-set1 (cdr set1) set2)))
        (else (intersection-set1 (cdr set1) set2))))
(define (union-set1 set1 set2)
  (cond ((or (null? set1) (null? set2)) set2)
        ((element-of-set1? (car set1) set2)
         (union-set1 (cdr set1) set2))
        (else (cons (car set1)
                    (union-set1 (cdr set1) set2)))))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
    (if (element-of-set? x set)
      set
      (cons x set)))
(define (union-set set1 set2)  
    (define (iter s p)
      (cond ((null? s) p)
            ((element-of-set? (car s) p)
             (iter (cdr s) p))
            (else (iter (cdr s) (cons (car s) p )))))
    (iter set2 (iter  set1  `())))
(define test `(1 2 3 4 3 2 1))
;(union-set test `(1 2 3 5))
(intersection-set1 test `(1 2 3 5))
(define (intersection-set set1 set2)
  (let ((t (intersection-set1 set1 set2)))
    (union-set t `())))
(intersection-set test  `(1 2 3 5))
           

  