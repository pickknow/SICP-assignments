#lang racket
(require "lib/stream.rkt")

(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-streams integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define (integral2 delayed-integrand initial-value dt)
  (stream-cons initial-value          
               (let ((integrand (force delayed-integrand)))
                 (integral2 (delay (stream-cdr integrand))
                             (+ (* dt  (stream-car integrand))
                                initial-value)
                             dt))))
(define (solve2 f y0 dt)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
(stream-ref (solve2 (lambda (y) y) 1 0.001) 1000)