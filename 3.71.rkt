#lang racket
(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))
(define (stream-top s n)
  (if (< n 0)
      (displayln `done)
      (begin
        (displayln (stream-car s))
        (stream-top (stream-cdr s) (- n 1)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))              
(define (W x y)
  (- (+ (car x) (cdr x))
     (+ (car y) (cdr y))))

(define (merge-weighted s1 s2 f)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (result (f s1car s2car)))
           (cond ((< result 0)
                   (stream-cons s1car (merge-weighted (stream-cdr s1) s2 f)))
                 (else
                  (stream-cons s2car (merge-weighted s1 (stream-cdr s2) f)))
                 )))))

(define (weighted-pairs s t f)
  (stream-cons
   (cons (stream-car s) (stream-car t))
   (merge-weighted    
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) f)
    f)))
(define a (weighted-pairs integers integers W))
(define (cube x) (* x x x))
(define (rW x y)
  (- (+ (cube (car x)) (cube (cdr x)))
     (+ (cube (car y)) (cube (cdr y)))))
(define (pair-weight s)
  (+ (cube (car s)) (cube (cdr s))))
(define b (weighted-pairs integers integers rW))
(define (ramanujan s)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s))))
    (if (= (pair-weight first) (pair-weight second))
        (stream-cons (pair-weight second)
                     (ramanujan (stream-cdr s)))
        (ramanujan (stream-cdr s)))))
(define c (ramanujan b))
(stream-top c 5)



