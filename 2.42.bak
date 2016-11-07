#lang racket
(require "lib/lib1.rkt")
(define empty-board `())
(define (adjoin-position row k queens)
  (if (not (pair? queens))
      (list (list k row))
      (append (list (list k row)) queens)))

(define (safe? k positions)
  (if (< k 2)
      #t
      (let ((row (caar positions))
            (col (cadar positions)))
        (= (accumulate + 0 (map (lambda (x)
                                  (let ((k-x (- k (car x))))
                                    (cond ((= col (cadr x)) 1)
                                          ((= col (+ (cadr x) k-x)) 1)
                                          ((= col (- (cadr x) k-x)) 1)
                                          (else 0))))
                                (cdr positions))) 0))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))