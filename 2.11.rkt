#lang racket
(define (mul-better x y)
  (let ((p1 (make-interval (min (lower-bound x) (upper-bound x))
                           (max (lower-bound x) (upper-bound x))))
        (p2 (make-interval (min (lower-bound y) (upper-bound y))
                           (max (lower-bound y) (upper-bound y)))))
    (make-interval (* (lower-bound p1) (lower-bound p2))
                   (* (upper-bound p1) (upper-bound p2)))))