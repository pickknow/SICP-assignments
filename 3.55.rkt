(define s integrs)
(define (partial-sums s)
  (cons-stream s (add-stream  partial-sums
                             s)))
