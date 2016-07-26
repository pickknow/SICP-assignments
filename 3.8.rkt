#lang racket
(define f
  (let ((count 1))
    (lambda (n)
      (set! count (* count n))
      count)))
(+  (f 1) (f 0))