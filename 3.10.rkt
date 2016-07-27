#lang racket
(define (squaer x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-if-squares (+ a 1) (+ a 2)))