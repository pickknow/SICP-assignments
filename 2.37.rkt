#lang racket
(require "lib/lib1.rkt")
(define test `((1 2 3 4) (4 5 6 6) (6 7 8 9)))



(define (map-deep op . z)
  (display z)
  (let ((initial (mapa z)))
       ((rest (mapb z)))
    (display rest)
       (append (list (list-op op initial))
               (map-deep op rest))))


(map-deep + `(1 2 3) `(10 20 30) `(100 200 300))

(define m (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)))

(define v (list 1 2 3 4))


(define (dot-product v w)
  (accumulate + 0 (map-deep * v w)))

(define (matrix-*-vector m v)
  (map (lambda(row) (dot-product v row)) m))
;(matrix-*-vector m v)