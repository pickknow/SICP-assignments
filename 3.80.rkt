#lang racket
(require "lib/stream.rkt")
(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-streams integrand dt)
                                int))))
  int)

(define (RC R C dt)
  (define (result i v)
    (add-streams (scale-streams i R)
                 (integral (scale-streams i (/ 1 C)) v dt)))
  result)
(define RC1 (RC 5 1 0.5))
(define v (RC1 integers 1))


(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (RLC R L C dt vc0 il0)
  (define vc (integral (delay dvc) vc0 dt))
  (define il (integral (delay dil) il0 dt))  
  (define dvc (scale-streams il (/ -1 C)  ))    
  (define a1 (scale-streams vc (/ 1 L) ))
  (define a2 (scale-streams il (-(/ R L)) ))
  (define dil (add-streams a1 a2))
  
  (zip-map cons vc il)
  )
(define a (RLC 1 0.2 1 0.1 0 10))
(stream-top a 10)
