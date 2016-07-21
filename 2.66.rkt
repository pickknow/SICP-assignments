#lang racket
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))
(define (lookup-tree g-key tree)
  (if (null? records)
      false
      (let ((k (key (entry tree))))
        (cond ((= g-key k) (entry tree))
              ((> g-key k) (lookup-tree g-key (right-branch tree)))
              ((< g-key k) (loopup-tree g-key (left-branch tree)))))))