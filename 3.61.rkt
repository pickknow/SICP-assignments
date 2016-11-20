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
(define (div-streams s1 s2)
  (zip-map / s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define ones (stream-cons 1 ones))

(define (integrate-series s)
  (mul-streams s
               (div-streams ones integers)))
(define a (integrate-series ones))

(define exp-series
         (stream-cons 1  (integrate-series exp-series)))
                              
(define (stream-step s)
     (stream-cdr (stream-cdr  s))) 
      
(define cosine-series
  (stream-cons 1
               (stream-cons (stream-car (stream-step exp-series))
               (stream-step (stream-cdr (stream-cdr exp-series))))))

               
(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams
                (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                             (scale-stream (stream-cdr s2) (stream-car s1)))
                (stream-cons 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))


               
