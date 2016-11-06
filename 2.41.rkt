#lang racket
(require "lib/lib1.rkt")
(define (have-three n s)
  (filter (lambda (x)
            (= (list-op + x) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval (+ j 1) n)))
                              (enumerate-interval (+ i 1) n)))
                   (enumerate-interval 1 n))))

(have-three 8 18)

