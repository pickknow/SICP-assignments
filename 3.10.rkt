#lang racket
(define (squaer x)
  (* x x))
(define (sum-of-sqrs x y)
  (+ (sqr x) (sqr y)))
(define (f a)
  (sum-if-sqrs (+ a 1) (+ a 2)))
