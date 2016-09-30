#lang racket
(define a (list (list 1 2) (list 3 4)))
(define (reverse x)
  (define (iter y r)
    (if (null? y)
        r
        (iter (cdr y) (list(car y) r))))
  (iter (cdr x) (car x)))
(define (deep-reverse x)
  (let ((a (reverse x)))
    (map reverse a)))
(reverse a)
(deep-reverse a)