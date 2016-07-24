#lang racket
(define (scheme-nimber->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercoin `scheme-number `complex scheme->number->complex)
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
                (if (not (eq? type1 type2))
                    (let ((who-up (up-who? type1 typp2)))
                      (cond ((eq?  who-up type1)
                             (apply-generic op ((get `raise type1) a1) a2))
                            ((eq?  who-up type2)
                             (apply-generic op a1 ((get `raise type2) a2)))                             
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types"
                           (list op type-tags))))
                (error "No method for these types"
                       (list op type-tags)))))))

(define (raise x)
  (let ((type type-tag x))
    (if (eq? type `complex)
        false
        (let ((proc (get `raise type)))
          (if proc
              (apply proc (contents x))
              false)))))
