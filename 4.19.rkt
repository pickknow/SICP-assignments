#lang racket
;增加一层扫描 将字面量赋值语句提前
(define (scan-out-defines proc)
  (define (self-definition? exp)
    (display exp)
    (and (definition? exp) (self-evaluating? (definition-value exp))))
  (define (scan-defines lst defines mul-defines bodys)
    (cond ((null? lst) (cons (append mul-defines defines) bodys))          
          ((self-definition? (car lst))
           (scan-defines (cdr lst) defines (append  mul-defines (list (car lst))) bodys))
          ((definition? (car lst))
           (scan-defines (cdr lst) (append defines (list (car lst))) mul-defines bodys))
          (else
           (scan-defines (cdr lst) defines mul-defines (append bodys (list (car lst)))))))
(define (make-lets lst)      
  (cons
   (map (lambda (x)
          (list (definition-variable x) ''*unassigned*))
        lst)
   (map (lambda (x)
          (list 'set! (definition-variable x)  (definition-value x)))
        lst)))
  (let ((defines (scan-defines proc `() `() `())))
     (list (append (cons 'let (make-lets (car defines)))
            (cdr defines)))))


