#lang racket
(define (make-unbound! var env)
  (variable-abs var val env
                (lambda (env-loop var val env vars vals)
                  true)
                (lambda (env-loop var val env vars vals)
                  (set-car! vals `())
                  (set-cdr! vals `()))))
  
