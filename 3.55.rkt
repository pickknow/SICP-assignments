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

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))



(define (partial-sums s)
  (stream-cons (stream-car s)
               (add-streams  (stream-cdr s)
                             (partial-sums s))))
(define a (partial-sums integers))
(stream-ref a 0)
(stream-ref a 1)
(stream-ref a 2)
(stream-ref a 3)
(stream-ref a 4)