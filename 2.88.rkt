#lang racket
(define (the-empty-termlist) `())
(define first-term car)
(define rest-terms cdr)
(define empty-termlists? null?)
(define make-term list)
(define order car)
(define coeff caddr)
;add a new term to list
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
              term-list
              (cons term term-list)))
(define (make-polynomial var terms)
  ((get `make `polynomial) var terms))
(define (add-poly p1 p2)
  (if (same-variable? (varialbe p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var --ADD-POLY"
             (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var --MUL-POLY"
             (list p1 p2))))
(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (else
         (let ((t1 (first-term l1)) (t2 (first-term l2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-term (rest-terms l1) l2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-term l1 (rest-terms l2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms l1)
                              (rest-terms l2)))))))))
(define (pass-term term)
  (make-term (order term) (- (coeff term))))
(define (sub-terms l1 l2)
  (cond ((empty-termlist? l2) l1)
        ((empty-termlist? l1)
         (map pass-term l2))
        (else
         (let ((t1 (first-term l1)) (t2 (first-term l2)))
           (cond ((> (order t1) (order t2)))
                 (adjoin-term
                  t1 (add-term (rest-term l1) l2))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   (pass-term t2) (add-term l1 (rest-terms l2))))
                  (else
                  (adjoin-term
                   (make-term (order t1) 0)
                   (add-terms (rest-terms l1)
                              (rest-terms l2)))))))))