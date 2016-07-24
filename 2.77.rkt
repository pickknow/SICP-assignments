#lang racket
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum --TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types --APPLY-GENERIC"
           (list op type-tags))))))
(define (add x y) (apply-generic `add x y))
(define (sub x y) (apply-generic `sub x y))
(define (mul x y) (apply-generic `mul x y))
(define (div x y) (apply-generic `div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag `scheme-number x))
  (put `add `(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put `make `scheme-number
       (lambda (x) (tag x)))
  `done)

(define (make-scheme-number n)
  ((get `make `scheme-number n)))

(define (install-complex-package)
  (define (tag z)
    (attach-tag `complex z))
  (put `add `(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  `done)