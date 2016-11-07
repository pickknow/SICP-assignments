#lang sicp
(#%require sicp-pict)
(#%require "lib/painter.rkt")

(define (split op1 op2)
  (lambda (painter n)
    (define (iter n)
      (if (= n 0)
          painter
          (let ((smaller (iter (- n 1))))
            (op1 painter (op2 smaller smaller)))))
    (iter n)))

(define right-split (split beside below))
(define up-split (split below beside))
(paint (up-split wave 2))