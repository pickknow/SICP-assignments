#lang racket
(for (variable start end)
   body)

(define (for? exp) (tagged-list exp `for))
(define for-variable caadr)
(define for-start cadadr)
(define for-end caddadr)
(define for-body cddr)
(define (for-iter start end proc)
  (if (< end start )
      (proc start)
      (for-iter (+ start 1) end proc)))
(define (for-combination exp)
  (for-iter
   (for-start exp)
   (for-end exp)
   (make-lambda (for-variable exp)
                (for-body exp))))

(while (predicate variable)
  body)
(define (while? exp) (tagged-list exp `while))
(define while