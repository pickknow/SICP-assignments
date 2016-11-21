#lang racket
(require math/base)
(require "lib/stream.rkt")
(define random-init 100)
(define (ran x)
  (random x 10000))
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

(define (ss dy)
  (define int
    (stream-cons  (ran dy)
                  (stream-map ran  int)))
  int)
(define (make-stream) 
  (define (x . args)
    (if (null? args)
         (ss random-init)
         (ss (car args))))
  x)

(define a (make-stream))
(stream-top (a) 5)
(a 1000)
(stream-top (a) 5)


