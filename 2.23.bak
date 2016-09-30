#lang racket
(define (for-each proc items)
  (cond ((not (null? items)) (proc (car items))))
  (if (null? items)
      `()     
      (for-each proc (cdr items))))
(for-each (lambda (x)
           (newline)
           (display x))
         (list 1 2 3 4 5))
     