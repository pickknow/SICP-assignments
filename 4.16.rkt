#lang racket
`(define (lookup-variable-value var env)
  (variable-abs var `() env
                (lambda (env-loop var val env vars vals)
                  (env-loop (enclosing-environment env)))
                (lambda (env-loop var val env vars vals)
                  (if (eq? *unassigned* (car vals))
                      (error "Unassigned" val)
                      (car vals)))))

(define str `(lambda ()
               (define u 1)
               (define v 2)
               (list `body)))

(define (scan-out-defines proc)
  (