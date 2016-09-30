#lang racket
(define (reverse s)
  (define (iter x r)
    (if (null? x)
        r
        (iter (cdr x) (cons (car x) r))))
  (iter (cdr s) (car s)))

(reverse (list 1 2 3 4 5))
