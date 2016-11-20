#lang racket
(require "lib/stream.rkt")

(define (stream-map proc . agrstreams)
  (if (stream-null? (car agrstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car agrstreams))
       (apply stream-map
              (cons proc (map stream-cdr agrstreams))))))
(define a (stream-enumerate-interval 1 10))
(define b (stream-enumerate-interval 11 20))
(define c (stream-enumerate-interval 21 30))
(define (displaylst . lst)
  (map display (list lst `/)))

(stream-map displaylst a b c)
                    