#lang racket
(define (square x) (* x x))
(define (repeated f n)
  (lambda (x)
    (define (iter k result)
      (if (= k 0)
          result
          (iter (- k 1) (f result))))
    (iter (- n 1) (f x))))
((repeated square 2) 5)

(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx))) 3)))
((smooth square) 5)
(((repeated smooth 5) square) 5)
  