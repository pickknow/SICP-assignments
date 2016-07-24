#lang racket
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
              term-list
              (cons term term-list)))
(define (the-empty-termlist) `())
(define first-term car)
(define rest-terms cdr)
(define empty-termlists? null?)
(define make-term list)
(define order car)
(define coeff caddr)

(define (make-polynomial var terms)
  ((get `make `polynomial) var terms))
(define (install-polynomial)
  (put `=zero? polynomial
       (lambda (poly)
         (define (=zero? x)           
           (cond ((and (number? x) (= x 0)) true)
                 ((null? x) false)
                 (else (=zero? (cdr x)))))
         (=zero? poly))))
