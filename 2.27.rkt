#lang racket
(define a (list (list 1 2) (list 3 4)))

(define (deep-reverse1 x)
  (let ((a (reverse x)))
    (map reverse a)))
(define (deep-reverse x)
  (cond ((null? x) null)
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))
(reverse a)
(deep-reverse a)