#lang racket
(require "lib/lib1.rkt")

(define a `(1 2 3 4))

(define (reserve-r seq)
  (fold-right (lambda (x y)
                (append y  (list x)))
              `()
              seq))
(reserve-r a)

(define (reverse-l seq)
  (fold-left (lambda (x y)
               (cons  y x))
             `()
             seq))
(reverse-l a)