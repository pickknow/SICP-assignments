#lang racket
(define (indentity x) x)
(define (inc x) (+ x 1))
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result a))))
(iter a 0))
(sum indentity 3 inc 4)