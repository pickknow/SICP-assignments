#lang racket
(define (indentity x) x)
(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
(integral cube 0 1 0.01)


(define (integral-tow f a b n)
  (define (get-pre n)
  (cond ((= n 0) 1)
        ((even? n) 2)
        (else 4))    
    )
  (define (get-yk a)
    (* (get-pre a) (f (+ a (* a (/ (- b a) n))))))
  (* 1 (sum f (get-yk a) get-yk b)))
(integral-tow cube 0 1 100)
    