#lang racket
(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(provide display-stream)
(define (display-stream s)
  (stream-for-each displayln s))
(define (zip-map proc . agrstreams)
  (if (null? (car agrstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car agrstreams))
       (apply zip-map
              (cons proc (map stream-cdr agrstreams))))))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(define (stream-limit s t)
  (let ((sub (stream-cdr s) ))
    (if (<= (abs (- (stream-car s) (stream-car sub))) t)
        (stream-car s)
        (stream-limit sub t))))
(define a (sqrt 2 0.0005))
a


        