#lang sicp
(#%require "lib/painter.rkt")
(#%require sicp-pict)
(define (make-segment v1 v2)
  (cons v1 v2))
(define start-segment car)
(define end-segment cdr)