#lang racket
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
(define test1 `(1))


(count-pairs (list test1 test1 test1))
(count-pairs `((1) 2 3))
(count-pairs `(((1)) (2) (3)))
