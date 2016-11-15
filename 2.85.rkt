#lang racket
 (define (install-scheme-number-package) 
  ;; ... 
  (put 'project 'scheme-number
    (lambda (x) (make-rational x 1)))
  
  'done)

(define (install-rational-package) 
  ;; ... 
  (put 'project 'rational
    (lambda (x) 
      (make-scheme (round (/ (numer x) (denom x))))))

  'done) 

(define (install-real-package)   
  ;; ... 
  (put 'project 'real
    (lambda (x) (round x)))

  'done)

(define (type-val t) 
  (cond
    ((eq? t 'scheme-number) 1)
    ((eq? t 'rational) 2)
    ((eq? t 'real) 3)
    ((eq? t 'complex) 4)
    (else (error "No such type" t))))

(define (raise-to-complex x)
  ;就是把x一直raise的complex为止，比较简单，这里忽略
  )
(define (equ?

(define (drop x)
  (let ((type type-tag x))
    (let ((proc (get `project type)))
      (if proc
          (let ((result (apply proc (contents x))))
            (let ((x2 (get `raise (type-tag result)) result))
            (if ((get `equ? type) x x2)
                (drop result)
                x)))
          x))))
(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                    (let ((who-up (up-who? type1 type2)))
                     (cond ((> who-up 0) (apply-generic op a1 (raise a2)))
                           ((< who-up 0) (apply-generic op (raise a1) a2))
                           (else (apply-generic op a1 a2)))))
                (error "No method for these types"
                       (list op type-tags)))))))



