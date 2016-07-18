#lang racket
(define (fa a count)
  (newline)
  (cond ((>= a count)
         (f 0 count 1)))
  (cond ((>= a count) 
      (fa a  (+ count 1)))))      
(define (f a b p)
  (cond ((> a 0) (print p)))
  (cond ((< a b)
         (f (+ a 1) b (g a (- b 1))))))
(define (g a b)
  (if (or (= a 0) (= a b))
      1
      (+ (g (- a 1) (- b 1)) (g a (- b 1)))))
(define (h n)
  (fa n 0))
(h 6)

      
