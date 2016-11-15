#lang racket
(define (make-f)
 (let ((init 2))
  (lambda (x)
   (begin (set! init (- init 1)) (* init x)))))
 (define f (make-f))
 (define f0 (f 0))
 (define f1 (f 1))
 (+ f0 f1)
 (define ff (make-f))
  (define ff1 (f 1))
  (define ff0 (f 0))
(+ ff0  1)

