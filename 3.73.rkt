#lang racket
(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (zip-map proc . agrstreams)
  (if (null? (car agrstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car agrstreams))
       (apply zip-map
              (cons proc (map stream-cdr agrstreams))))))
(define (add-streams s1 s2)
  (zip-map + s1 s2))
(define (mul-streams s1 s2)
  (zip-map * s1 s2))
(define (scale-streams s fac)
  (stream-map (lambda (x) (* x fac)) s))
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (stream-top s n)
  (if (< n 0)
      (displayln `done)
      (begin
        (displayln (stream-car s))
        (stream-top (stream-cdr s) (- n 1)))))

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-streams integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (result i v)
    (add-streams (scale-streams i R)
                 (integral (scale-streams i (/ 1 C)) v dt)))
  result)
(define RC1 (RC 5 1 0.5))
(define v (RC1 integers 1))
(stream-top v 11)

           