#lang R5RS
(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x)
  (* x x))
(define (find-divisor n t)
  (cond ((> (square t) n) n)
        ((divides? t n) t)
        (else (find-divisor n (+ t 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))



(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)