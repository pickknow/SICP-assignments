#lang racket
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (iterative good? im)
  (lambda (x)
    (define (iter guess)
      (if (good? guess x)
          guess
          (iter (im guess x))))
    (iter 1)))
(define (sqrt-iter2 x)
  ((iterative good-enough? improve) x))
(sqrt-iter2 16)