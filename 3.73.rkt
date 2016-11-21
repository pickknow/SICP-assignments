#lang racket
(require "lib/stream.rkt")

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-streams integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (result i v)
    (add-streams (scale-streams i R)
                 (integral (scale-streams i (/ 1 C)) v dt)))
  result)
(define RC1 (RC 5 1 0.5))
(define v (RC1 integers 1))
(stream-top v 11)

           