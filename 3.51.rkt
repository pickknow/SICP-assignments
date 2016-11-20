#lang racket
(require "lib/stream.rkt")

(define (show x)
  (displayln x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)