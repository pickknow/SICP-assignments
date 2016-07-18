#lang racket
(define (f n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (* 2 (f (- n 1))))))
(f 9)
(define (h n)
  (define (h-h x)
    (cond
      ((= x 0) 1)
      (else (* 2 x))))
  (define (h-iter n count)
    (if (> n 0)       
        (h-iter (- n 1) (h-h count))
         count
        ))
  (h-iter n 0))
(h 9)