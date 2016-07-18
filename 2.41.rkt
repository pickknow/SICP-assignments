#lang racket
(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (enumerate-interval a b)
  (define (iter x r)
    (if (> x b)
        r
        (iter (+ x 1) (append r (list x)))))
  (iter  a  `()))

(define (accumulate op initial sequence) 
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append `() (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (unique-pairs n)
  (flatmap
   (lambda (i)
         (map (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
(define (have3 n)
  (flatmap (lambda (x)
              (map
               (lambda (i)
                 (append x (list i)))
               (enumerate-interval 1 (- (cadr x) 1))))
       (filter
        (lambda (z)
              (not (< (cadr z) 2)))  
        (unique-pairs  n))))
(have3 4)
(car (have3 4))
(append `(((1 2 3))) `((2 3 4)))
(accumulate append null (have3 4))
(define (filter3 n)
  (filter (lambda (i)         
          (not (null? (cddr i))))
       (have3 n)))
;(filter3 4)
(define (uni-sum s n)
 (filter (lambda (x) (= s (accumulate + 0 x)))
         (filter3 n)))
  
(uni-sum 10 9)  

