#lang racket
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-car! (last-pair x) x))
(define z (make-cycle `(a b c)))
`(a b a b c)