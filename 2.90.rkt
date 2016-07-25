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
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (varialbe p1)
                 (sub-term (term-list p1)
                           (term-list p2)))
      (error "Polys not in same var --SUB-POLY"
             (list p1 p2))))
(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-term (term-list p1)
                           (term-list p2)))
      (error "Polys not in same var --DIV-POLY"
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
(define (div-term l1 l2)
  (cond ((empty-termlist? l2) l1)
        ((empty-termlist? l2) l2)
        (else
         (let ((t1 (first-term l1)) (t2 (first-term l2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1
                               (div-term (rest-terms l1) l2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2
                               (div-term l1 (rest-terms l2))))
                 (else
                  (adjoin-term
                   (make-term (oerder t1) 1)
                   (div-term (rest-terms l1)
                             (rest-terms l2)))))))))
;以下为稀疏多项式  sparse
;稠密多项式terms((100 1) (2 2) (0 1))
;sparse (1 2 0 3 -2 -5)
(define (len poly-sparse) (- (length poly-sparse) 1))
(define (make-poly-sparse x y) (cons x y))
(define (make-term-sparse  x y)
  (let ((a (if (pair? x) x (list x)))
        (b (if (pair? y) y (list y))))
    (append x y)))
(define (add-poly-sparse l1 l2)
  (if (same-variable? l1 l2)
      (make-poly-sparse (variable l1)
                        (add-term-sparse (term-list l1)
                                         (term-list l2)))
      (error "Polys not in same variable --ADD-POLY-SPARSE"
             （list l1 l2)))
(define (add-term-sparse l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l2)
        (else
         (let ((t1 (len l1)) (t2 (len l2)))
           (cond ((> t1 t2)
                  (adjoin-term (car l1)
                               (add-term-sparse (cdr l1) l2)))
                 ((< t1 t2)
                  (adjoin-term (car l2)
                               (add-term-sparse l1 (cdr l2))))
                 (else
                  (adjoin-term
                   (add (car l1) (car l2))
                   (add-term-sparse (cdr l1) (cdr l2)))))))))

