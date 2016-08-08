(define (integrate-series C)
  (cons-stream C
               ((lambda (x)
                 (/ 1 x))
                integrs)))
(define exp-series
         (cons-stream 1 (interate-serise exp-series)))
(define cosine-series
  (cons-stream 1 
