#lang racket
(define (print-rat x)
  (newline)
  (display (start-segment x))
  (display ",")
  (display (end-segment x)))
(define (make-segment x y) (cons x y))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point y) (cdr y))

(define (midpoint-segment x)  
  (let ((midx (/ (+ (car (car x)) (car (cdr x))) 2))
        (midy (/ (+ (cdr (car x)) (cdr (cdr x))) 2)))
        (cons midx midy)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment
              (make-segment (make-point 1 1)
                            (make-point 3 3))))

