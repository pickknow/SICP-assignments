#lang racket
(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (zip-map proc . agrstreams)
  (if (null? (car agrstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car agrstreams))
       (apply zip-map
              (cons proc (map stream-cdr agrstreams))))))
(define (add-streams s1 s2)
  (zip-map + s1 s2))
(define (mul-streams s1 s2)
  (zip-map * s1 s2))
(define (scale-streams s fac)
  (stream-map (lambda (x) (* x fac)) s))
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (stream-top s n)
  (if (< n 0)
      (displayln `done)
      (begin
        (displayln (stream-car s))
        (stream-top (stream-cdr s) (- n 1)))))
(define (sign-change-detector x y)
  (display (list x y))
  (cond ((and (> x 0) (<= y 0)) -1)
        ((and (< x 0) (>= y 0)) 1)
        (else 0)))

(define (make-zero-crossings1 input-stream last-value)
  (stream-cons
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define sense-data
  (stream-cons 1 (stream-map - sense-data)))
(define zero-crossings
  (zip-map sign-change-detector sense-data
              (stream-cons 0 zero-crossings)))

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (stream-cons (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      last-value last-avpt))))
(define zero-crossings1 (make-zero-crossings sense-data 0 0))
(stream-top zero-crossings1 5)
