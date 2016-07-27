#lang racket
(define a `(1))
(define b (list a a a))

(define (count-pairs x)
  (let ((acc `()))
    (define (loop y)
      (cond ((null? y) 0)
            ((not (pair? y))
             (if (memq y acc)
                 0
                 (begin (set! acc (cons y acc)) 1)))
             (else
              (+ (loop (car y))
                 (loop (cdr y))))))
      (loop x)))
(count-pairs b)


          