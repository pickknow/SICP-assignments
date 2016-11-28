(load "keyword.scm")

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))



(define (true? x)
  (if (number? x)
      (> x 0)
      (not (eq? x false))))
(define (false? x)
  (eq? x false))

(define (contain-defines exps) 
   (if (null? exps) 
     false 
     (or (if (definition? (car exps)) 
           true 
           false) 
         (contain-defines (cdr exps)))))
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
(define (make-procedure parameters body env) 
  (if (contain-defines body) 
      (list 'procedure parameters (scan-out-defines body) env) 
      (list 'procedure parameters body env))) 
;(define (make-procedure parameters body env)
;  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list 'display display)
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


