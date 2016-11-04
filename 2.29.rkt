#lang racket
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
;a
;(define left-branch car)
;(define right-branch cadr)
;(define branch-length car)
;(define branch-structure cadr)

;b
(define (total-weight m)
  (let ((lb (left-branch m))
        (rb (right-branch m)))   
    (cond ((and (pair? (branch-structure lb)) (pair? (branch-structure rb)))
           (+ (total-weight (branch-structure lb))
              (total-weight (branch-structure rb))))
          ((and (pair? (branch-structure lb)) (not (pair? (branch-structure rb))))
           (+ (total-weight (branch-structure lb))
              (branch-structure rb)))
          ((and (not (pair? (branch-structure lb))) (pair? (branch-structure rb)))
           (+ (branch-structure rb)
              (total-weight (branch-structure rb))))
          (else
           (+ (branch-structure lb)
              (branch-structure rb))))))

(define test (make-mobile (make-branch 2 3) (make-branch 2 3)))
;(display `b-test)
;(total-weight test)

;c
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))
(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))
(define (branch-balance? branch)
  (if (pair? (branch-structure branch))
      (balance? (branch-structure branch))
      #t))
(define (balance? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (branch-balance? (left-branch mobile))
       (branch-balance? (right-branch mobile))))

;(balance? test)


;d
(define (make-mobile-cons left right)
  (cons left right))
(define (make-branch-cons length structure)
  (cons length structure))
;a2
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)
(define test-cons (make-mobile-cons (make-branch-cons 2 3) (make-branch-cons 2 3)))
;b2
(total-weight test-cons)
;c  
(balance? test-cons)


