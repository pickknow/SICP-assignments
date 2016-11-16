#lang racket
(define a `(1))
(define b (list a a a))

(define (count-pairs x)
  (let ((acc `()))
    (define (loop y)
      (cond ((not (pair? y)) 0)
            ((memq y acc) 0)
            (else
             (set! acc (cons y acc))             
             (+ 1 (loop (car y))
                (loop (cdr y))))))
    (loop x)))
(count-pairs b)
(count-pairs (cons (cons 1 2) (cons 3 4)))
(count-pairs (let ((x (cons 1 2)))
	       (cons x x)))
(define p1 (cons 'a null))
(define p2 (cons p1 null))
(define p3 (cons p1 p2))
(count-pairs p3)
;Value: 3

(count-pairs p1)
          
