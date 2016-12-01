(define (procedure-parameters-ex4.31 p) 
   (define (name parameter) 
     (if (pair? parameter) 
       (car parameter) 
       parameter)) 
   (define (parameter-names parameters) 
     (if (null? parameters) 
       '() 
       (cons (name (car parameters)) 
             (parameter-names (cdr parameters))))) 
   (parameter-names (cadr p))) 
 (define (procedure-raw-parameters p) (cadr p)) 
  
 (define (apply-ex4.31 procedure arguments env) 
   (cond [(primitive-procedure? procedure) 
          (apply-primitive-procedure 
            procedure 
            (list-of-arg-values arguments env))]                   ; changed 
         ((compound-procedure? procedure) 
          (eval-sequence 
            (procedure-body procedure) 
            (extend-environment 
              (procedure-parameters procedure) 
              (list-of-delayed-args (procedure-raw-parameters procedure) arguments env) 
              (procedure-environment procedure))))                 ; changed 
         (else (error "Unknow procedure type: APPLY" 
                      procedure)))) 
  
 (define (list-of-delayed-args-ex4.31 raw_parameters exps env) 
   (define (arg-value raw_parameter exp) 
     (if (pair? raw_parameter) 
       (cond ((eq? (cadr raw_parameter) 'lazy) 
              (delay-it-no-memo exp env)) 
             ((eq? (cadr raw_parameter) 'lazy-memo) 
              (delay-it exp env)) 
             (else (error "Unknow parameter type LIST-OF-DELAYED-ARGS:" (cadr raw_parameter)))) 
       (actual-value exp env))) 
   (if (no-operands? exps) 
     '() 
     (cons (arg-value (car raw_parameters) 
                      (first-operand exps)) 
           (list-of-delayed-args-ex4.31 (cdr raw_parameters) 
                                 (rest-operands exps) 
                                 env)))) 
  
 (define (delay-it-no-memo exp env) 
   (list 'thunk-no-memo exp env)) 
 (define (thunk-no-memo? obj) 
   (tagged-list? obj 'thunk-no-memo)) 
  
 (define (force-it-ex4.31 obj) 
   (cond ((thunk? obj) 
          (let ((result (actual-value (thunk-exp obj) 
                                      (thunk-env obj)))) 
            (set-car! obj 'evaluated-thunk) 
            (set-car! (cdr obj) 
                      result)       ; replace exp with its value 
            (set-cdr! (cdr obj) 
                      '()) 
            result)) 
         ((evaluated-thunk? obj) (thunk-value obj)) 
         ((thunk-no-memo? obj) (actual-value (thunk-exp obj) 
                                             (thunk-env obj))) 
         (else obj)))       ; forget unneeded env 
  
  
 (define apply apply-ex4.31) 
 (define force-it force-it-ex4.31) 
 (define procedure-parameters procedure-parameters-ex4.31) 
 (define list-of-delayed-args list-of-delayed-args-ex4.31) 
  
 ;(define (id x) 
 ;  (set! count (+ count 1)) x) 
 ; 
 ; (define count 0) 
 ; 
 ; (define (square x) (* x x)) 
 ; 
 ; (square (id 10)) 
 ; 
 ; count ; 1 
 ; 
 ; (define count 0) 
 ; 
 ; (define (square (x lazy)) (* x x)) 
 ; 
 ; (square (id 10)) 
 ; 
 ; count ; 2 
 ; 
 ; (define count 0) 
 ; 
 ; (define (square (x lazy-memo)) (* x x)) 
 ; 
 ; (square (id 10)) 
 ; 
 ; count ; 1 
 ; end exercise 4.31 