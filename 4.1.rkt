#lang racket
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (list-of-values-left exps env)
  (if (no-operands? exps)
      `()
      (let ((frist (eval (first-operand exps) env)))
        (cons frist
              (list-of-values-left (rest-operands exps) env)))))
(define (list-of-value-right exps env)
  (if (no-operands? exps)
      `()
      (let ((right (eval (rest-operands exps) env)))
        (cons (eval (first-operands exps) env)
              right))))

      