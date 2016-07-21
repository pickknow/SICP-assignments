#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
 
(define (identity x) x)
(define (inc x) (+ x 1))
(define (plus x y) (+ x y))
(define (mul x y) (* x y))
 (accumulate plus 0 identity 1 inc 4)
(accumulate mul 1 identity 1 inc 4)
(define (accu-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
(accu-iter mul 1 identity 1 inc 4)


