#lang racket
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op `real-part) x)
          ((eq? op `imag-part) y)
          ((eq? op `magnitude)
           (sqrt (+ (squars x) (square y))))
          ((eq? op ` angle) (atan y x))
          (else
           (error "Unknow op --MAKE-FROM-REAL-IMAGE" op))))
  dispach)

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op `real-part)
           (* x (cos y)))
          ((eq? op `imag-part)
           (* x (sin y)))
          ((eq? op `magnitude) x)
          ((eq? op `angle) y)
          (else
           (error "Unknow op --MAKE-FROM-MAG-ANG" op)))))
            