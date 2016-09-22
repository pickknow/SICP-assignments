#lang racket
(define (tan-cf d x k)
  (define (try z)
    (if (> z k)
        (d x)
        (- z (/ (d x) (try (+ z 2))))))
  (/ x (try 1)))
(tan-cf (lambda (i) (* i i))
          4.0
          100)
(tan 4)