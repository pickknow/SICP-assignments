#lang racket
(require "lib/lib1.rkt")
(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(define v (list 1 2 3 4))

(define (dot-product v w)
  (accumulate + 0 (map-deep * v w)))

(define (matrix-*-vector m v)
  (map (lambda(row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons `() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))
         m)))

(transpose m)
(dot-product v v)
(matrix-*-vector m v)
(matrix-*-matrix m m)

