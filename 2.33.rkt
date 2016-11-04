#lang racket
(define test (list 1 2 3 4 5))
(define (sequare x) (* x x))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y)
               (cons (p x) y))                
             `()
             sequence))
(map sequare test)
(define (append seq1 seq2)
  (accumulate cons seq1 seq2))
(append test (map sequare test))
(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))
(length test)
(length (append test (map sequare test)))