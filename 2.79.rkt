#lang racket
(define (sub x y) (apply-generic `sub x y))
(define (mul x y) (apply-generic `mul x y))
(define (div x y) (apply-generic `div x y))
(define (=equ? x y) (apply-generic `=equ? x y))
(define (=zero? x ) (apply-generic `=zero? x))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag `scheme-number x))
  (put `add `(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put `make `scheme-number
       (lambda (x) (tag x)))
  (put `=equ? `(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put `=zero? `(scheme-number scheme-number)
       (lambda (x y) (and (= x 0) (= y 0))))
  `done)
;and so on others
(define (equ? x)
 (apply-generic (type-tag x) (contents x)))

