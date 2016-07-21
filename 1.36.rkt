#lang racket
(define tolerance 0.0001)
(define (average x y) (/ (+ x y) 2))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (sqrt-x x)
  (fixed-point (lambda (y) (/ (log 1000) (log y)))
               2))
(sqrt-x 1000)
  (define (sqrt-y x)
  (fixed-point (lambda (y) (average y (/ (log 1000) (log y))))
               2))
(sqrt-y 1000)



