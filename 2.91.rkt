#lang racket
;返回两个表 商 和余
(define (div-terms l1 l2)
  (if (empty-termlist? l1)
      (list (the-tmpey-termlist) (the-empty-termlist))
      (let ((t1 (first-term l1))
            (t2 (first-term l2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) l1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (lambda (x y)
                       
                       
                     ))
                (list l1 
                ))))))