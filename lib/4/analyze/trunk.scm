(load "util.scm")
(define (delay-it exp env)
 (list 'trunk exp env))
(define (thunk? exp)
 (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
 (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
 (cond ((thunk? obj)
        (let ((result (actual-value
                       (thunk-exp obj)
                       (thunk-env obj))))
         (set-car! obj 'evaluated-thunk)
         (set-car! (car obj) result)
         (set-cdr! (car obj) '())
         result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))