(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-stream (scale-stream integrand dt)
                             int)))
  int)
(define (RC R C dt)
  (define (result i v)
    (add-stream (scale-stream i R)
                (add-stream (scale-stream i (/ 1 C))
                            v)))
  result)

           