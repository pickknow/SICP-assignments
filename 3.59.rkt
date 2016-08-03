(define (integrate-series C)
  (cons-stream C
               ((lambda (x)
                 (/ 1 x))
                integrs)))
