#lang racket
(define test (list 1 (list 2 (list 3 4))))
test
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
(define (inc-tree-map tree)
  (map (lambda (x)
         (if (not (pair? x))
             (inc x)
             (inc-tree-map x)))
       tree))             
(define (inc x) (+ x 1))
(inc-tree-map  test)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append `(1 2) `(3 4))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
                0 sequence))
(length `(1 2 3 4))
                