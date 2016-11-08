#lang racket
;union-set  并集
;intersection-set 交集
;element-of-set?
;adjoin-set 加入新集合
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
      (cons x set))
(define (union-set set1 set2)  
    (append set1 set2))


;(union-set test `(1 2 3 5))
(define (remove-set x set)
  (if (element-of-set? x set)
      (cond ((eq? x (car set))
             (remove-set x (cdr set)))
            (else (cons (car set)
                        (remove-set x (cdr set)))))
      set))
(define (intersection-set set1 set2)
  (let ((set (union-set set1 set2)))
    (define (iter rest)
      (cond ((null? rest) `())
            ((element-of-set? (car rest) (cdr rest))
             (cons (car rest) (iter (remove-set (car rest) (cdr rest)))))
            (else
             (cons (car rest) (iter  (cdr rest))))))
    (iter set)))
 (define a `(1 2 3 4 3 2 1))
(define b `(3 5 6 7 1))
(intersection-set a b)


           

  