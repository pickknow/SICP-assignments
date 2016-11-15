#lang racket
(define (up-who? type1 type2)
  (define (type-val t)
   (cond ((eq? t `scheme-number) 0)
         ((eq? t `rational) 1)
         ((eq? t `real) 2)
         ((eq? t `complex) 3)
         (else (error "NO SUCH TYPE" t))))
  (- (type-val type1) (type-val type2)))


(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                    (let ((who-up (up-who? type1 type2)))
                     (cond ((> who-up 0) (apply-generic op a1 (raise a2)))
                           ((< who-up 0) (apply-generic op (raise a1) a2))
                           (else (apply-generic op a1 a2))))))
                (error "No method for these types"
                       (list op type-tags))))))


