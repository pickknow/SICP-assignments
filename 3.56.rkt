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

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (scale-stream stream factor)
  (zip-map (lambda (x) (* x factor)) stream))
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                   (stream-cons s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (stream-cons s2car (merge s1 (stream-cdr s2))))
                  (else
                   (stream-cons s1car (merge (stream-cdr s1)
                                             (stream-cdr s2)))))))))
(define S (stream-cons 1 (merge (scale-stream S 2)
                               (merge (scale-stream S 3)
                                      (scale-stream S 5)))))
