#lang racket
(require "lib/eval.rkt")
(require "lib/table.rkt")
(define (and-or-left exp) (cadr exp))
(define (and--or-right exp) (caddr exp))
(put `and? (lambda (exp env)
             (let ((result (eval (and-or-left exp) env)))
                (if result
                    (eval (and--or-right exp) env)
                    false))))

(put `or? (lambda (exp env)
             (let ((result (eval (and-or-left exp) env)))
                (if result
                    result
                    (eval (and--or-right exp) env)))))