(load "util.scm")

; 赋值的形式 (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; 定义的形式
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; lambda 表达式是由符号 lambda 开始的表

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

; lambda 构造函数
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; if 条件式

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

; if 构造函数

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


; 条件
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

; 序列，可以用在 apply 用于求值过程体里的表达式序列，也可以用在 eval 求值 begin 表达式里面的表达式序列

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; 赋值和定义

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (let? exp) (tagged-list? exp 'let))
(define (let-special? exp) (symbol? (cadr exp)))
(define (let-special-name exp)(cadr exp))
(define (let-list exp)
  (if (let-special? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (let-special? exp)
      (cadddr exp)
      (cddr exp)))
(define (let-variables exp)
  (map car (let-list exp)))
(define (let-values exp)
  (map cadr (let-list exp)))
(define (make-let-normal exp)
     (cons (make-lambda
   		(let-variables exp)
		(let-body exp))
	(let-values exp)))

(define (make-let-named exp)
  (list 'let (let-list exp)
        (list 'define (cons (let-special-name exp)
                            (let-variables exp))
              (let-body exp))
        (cons (let-special-name exp) (let-variables exp))))

(define (make-let-named2 expression)
  (let* ((var-name (cadr expression))
         (let-list (caddr expression))
         (arg-list (map car let-list))
         (arg-init (map cadr let-list))
         (let-body (cdddr expression)))
    (list 'let
          (list (list var-name
                      (make-lambda arg-list let-body)))
          (cons var-name arg-init))))

(define (let->combination exp)
  (if (let-special? exp)
      (make-let-named exp)
      (make-let-normal exp)))
