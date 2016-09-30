#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (factor num base)
  (define (iter a count)
    (if (= (remainder a base) 0)        
        (iter (/ a base) (+ count 1))
        count
        ))
  (iter num 0))
(define (car x)
  (factor x 2))
(define (cdr x)
  (factor x 3))
(car 108)
(cdr 108)


