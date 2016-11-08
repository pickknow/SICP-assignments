#lang racket
;(#%require sicp-pict)
(require "lib/qiudao.rkt")



(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) `**)))

(define base cadr)

(define exponent caddr)

(define (make-exponetiation base exponent)
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list `** base exponent))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
         ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponetiation (exponent exp)
                               `(- (base exp) 1)))
          
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))
(deriv `(+ x 3) `x)
(deriv `(* x y) `x)
(deriv '(* (* x y) (+ x 3)) 'x)