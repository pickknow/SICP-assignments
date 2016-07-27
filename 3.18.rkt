#lang racket
(define (check-cycle x)
  (eq? (last-pair x) x))
(define a `(1 2))
