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
                    (let ((t1->t2 (get-coercoin type1 type2))
                          (t2->t1 (get-coercoin type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types"
                           (list op type-tags))))
                (error "No method for these types"
                       (list op type-tags)))))))