#lang racket
(require "lib/qiudao.rkt")
(define (augend m)
  (let ((rest (cddr m)))
    (if (null? (cdr rest))
        (car rest)
        (cons `+ rest)
        )))

(define (multiplicand m)
    (let ((rest (cddr m)))
      (if (null? (cdr rest))
          (car rest)
          (cons `*  rest))))

(deriv `(* (* x y) (+ x 3)) `x)
(define a `(* x y (+ x 3)))
(deriv a `x)

