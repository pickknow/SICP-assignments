#lang racket
(define test (list 1 2 3 4 5))
(define (sequare x) (* x x))
(define (fliter predicate sequence)
  (cond ((null? sequence) `())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(filter odd? test)
(accumulate + 0 test)
(define (enumerate-interval low high)
  (if (> low high)
      `()
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
(define (enumerate-tree tree)
  (cond ((null? tree) `())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 (list 4 5)))))
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map sequare
                   (filter odd?
                           (enumerate-tree tree)))))
(sum-odd-squares test)