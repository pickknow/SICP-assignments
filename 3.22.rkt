#lang planet neil/sicp
(define (make-queue)
 (let ((front-ptr nil)
       (rear-ptr nil))
  (define (insert! x)
   (let ((new-pair (cons x nil)))
   (cond ((null? front-ptr)
          (set! front-ptr new-pair)
          (set! rear-ptr new-pair)
          front-ptr)
         (else
          (set-cdr! rear-ptr new-pair)
          (set! rear-ptr new-pair)
          front-ptr))))
  (define (delete!)
   (cond ((null? front-ptr)
          (error "DELETE! called an empey queue" front-ptr))
         (else
          (set! front-ptr (cdr front-ptr))
          front-ptr)))
  (define (dispatch m)
   (cond ((eq? m `insert!) insert!)
         ((eq? m `delete!) (delete!))))
  dispatch))

 (define a (make-queue))
 ((a `insert!) 1)
 ((a `insert!) 2)
 ((a `insert!) 3)
 (a `delete!)

