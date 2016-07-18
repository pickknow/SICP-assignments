#lang racket
(define (sumsquare a b)
  (+ (* a a) (* b b)))
(define (>= a b)
  (not (< a b)))
(define (maxTwo a b c)
  (cond
    ((and(>= a b) (>= a c)) (sumsquare a (max b c)))
    ((and(>= b a) (>= b c)) (sumsquare b (max a c)))
    (else (sumsquare c (max a b)))))
(maxTwo 5 2 -3)
