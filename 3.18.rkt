#lang planet neil/sicp
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (loop? x)
  (let ((identity (cons `() `())))
        (define (iter y)
          (cond ((null? y) #f)
                ((eq? (car y) identity) #t)
                (else
                 (set-car! y identity)
                 (iter (cdr y)))))
        (iter x)))


(define loop (list 1 2 3))
(loop? loop)
(define loop-list (list 1 2 3))
(set-cdr! (last-pair loop-list) loop-list)
(loop? loop-list)