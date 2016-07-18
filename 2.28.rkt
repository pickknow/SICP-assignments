#lang racket
(define a (list (list 1 2) (list 3 4) ))
(define (firnge x)
  (define (iter y r)
      (if (or (null? y) (not (pair? (car y))))
      r
     (iter (cdr y) (append r (car y)))))
  (iter (cdr x) (car x)))
(define (firnge2 x)
  (if (or (null? x) (not (pair? (car x))))
       x
       (append (firnge2 (car x)) (firnge2 (cdr x)))))
(firnge2 a)
(firnge2 (list a a))