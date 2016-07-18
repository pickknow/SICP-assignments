#lang racket
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)

(define (total-weight x)
  (if (not (pair? (branch-structure x)))
      (branch-structure x)
      (+ (total-weight (left-branch x))
         (total-weight (right-branch x)))))

(define test (cons (cons 1 2) (cons 2 1)))
 (total-weight test)
(define (mul x)
  (* (branch-length x) (total-weight x)))
(define (banlance? x)
  (= (mul (left-branch x))
     (mul (right-branch x))))
(define (is-active? x)
  (and (pair?  (left-branch x))
       (pair?  (right-branch x))))
(is-active? test)
(banlance? test)
(define (check-active x)
  (let* ((l (left-branch x))
        (r (right-branch x))
        (ls (branch-structure l))
        (rs (branch-structure r)))
    
  (and (cond ((is-active? l) (check-active ls)))
       (cond ((is-active? r) (check-active rs)))
       (banlance? x))))

(check-active test)


