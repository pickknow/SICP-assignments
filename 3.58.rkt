(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) dan) dan radix)))
(expand 1 7 10)
; 0 0 0
(expand 3 8 10)
;2 2 2 2
   