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
(define (for->combination exp)
  (for-iter
   (for-start exp)
   (for-end exp)
   (make-lambda (for-variable exp)
                (for-body exp))))

(while (predicate variable)
  body)
(define (iter predicate variable)
  (if (predicate variable)
      (body)
      
(define (while? exp) (tagged-list exp `while))
(define while-predicate caadr)
(define while-variable cadadr)
(define while-body cddr)


(define (while->combination exp)
  (sequence->exp
   (lsit (list `define
               (list `while-iter
                     (make->if ((while-predicate exp)
                                (while-variable exp))
                               (sequence->exp (list (while-body exp)
                                                    (list `while-iter))))))
         (list `while-iter))))
 
 (define (while? expr) (tagged-list? expr 'while)) 
(define (while-condition expr) (cadr expr))
 (define (while? exp) (tagged-list? exp 'while)) 
 (define (while-pred exp)(cadr exp)) 
 (define (while-actions exp) (caddr exp))   
 (define (make-single-binding var val)(list (list var val))) 
 (define (make-if-no-alt predicate consequent)(list 'if predicate consequent)) 
 (define (make-combination operator operands) (cons operator operands)) 
 (define (while->combination expr) 
   (sequence->exp 
    (list (list 'define  
                (list 'while-iter) 
                (make-if (while-condition expr)  
                         (sequence->exp (list (while-body expr)  
                                              (list 'while-iter))) 
                         'true)) 
          (list 'while-iter)))) 
 (define (while->rec-func exp) 
   (list 'let (make-single-binding 'while-rec '(quote *unassigned*)) 
         (make-assignment
          'while-rec 
          (make-lambda
           '() 
           (list (make-if-no-alt  
                  (while-pred exp) 
                  (make-begin
                   (append
                    (while-actions exp) 
                    (list (make-combination 'while-rec '())))))))) 
         (make-combination 'while-rec '()))) 
  
  