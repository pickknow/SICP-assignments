#lang racket
(define (list-pair a)
  (if (= (length a) 1)
      a
      (list-pair (cdr a))))
(last-pair `(1 2 3 4))