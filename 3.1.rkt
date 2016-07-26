#lang racket
(define (make-accumulator init)
  (let ((accumulator init))
    (lambda (x)
      (set! accumulator (+ accumulator x))
      accumulator)))
(define A (make-accumulator 5))
(A 10)
(A 10)