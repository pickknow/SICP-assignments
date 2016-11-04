#lang racket
(require "lib/lib1.rkt")
(define testval (list 'a (list 'b 'c 'd 'e) 'f (list 'g 'h (list 'i (list 'j 'k) 'l))))

(define (count-leave-original x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leave-original (car x))
                 (count-leave-original (cdr x))))))

(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (x) 1)
          (enumerate-tree t))
         ))
(count-leaves testval)
