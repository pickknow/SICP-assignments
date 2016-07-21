#lang racket
(define (mul-p x y)
  (let ((a (percent x))
        (b (percent y)))
    (- (* (+ 1 a) (+ 1 b))
       (* (- 1 a) (- 1 b)))))
  