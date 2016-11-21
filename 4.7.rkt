#lang racket
(require "lib/eval.rkt")
(define (let*? exp)
  (tagged-list? exp `let*))
(define (let*-body exp)
  (caddr exp))
(define (let*-variables exp)
  (cadr exp))
(define (let*->nested-lets exp)
  (let ((inits (let-variables exp))
        (body (lset-body exp)))
    (define (iter lets)
      (if (null? lets)
          body
          (iter (list `let (list (car lets)) (iter (cdr lets))))))
    (iter inits)))
        



