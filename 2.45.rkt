#lang racket
(define (split op1 op2)
  (lambda (painter n)
    (define (iter x)
      (if (= x 0)
          painter
          (let ((smaller (iter (- x 1))))
            (op1 painter (op2 smaller)))))
    (iter n)))