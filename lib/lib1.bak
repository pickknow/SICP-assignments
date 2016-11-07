#lang racket/base
(provide accumulate)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (check-car? seqs)
  (cond ((null? seqs) #f)
        ((null? (car seqs)) #t)
        (else
         (check-car? (cdr seqs)))))
(define a `((1 2 3 4) (4 5 6 ) (6 7 8 )))

(provide fold-right)
(define fold-right accumulate)

(provide fold-left)
(define (fold-left op initial sequence)
  (define (iter result seq)
    (if (null? seq)
        result
        (iter (op result (car seq))
              (cdr seq))))
  (iter initial sequence))


(provide accumulate-n)
(define (accumulate-n op init seqs)
  (if (or (null? (car seqs)) (check-car? seqs))
      `()
      (cons (accumulate op init  (mapa seqs))
            (accumulate-n op init (mapb seqs)))))
(provide mapa)
(define (mapa seqs)
  (map (lambda (x)
         (car x))
       seqs))
(provide mapb)
(define (mapb seqs)
  (map (lambda (x)
         (cdr x))
       seqs))

(provide enumerate-tree)
(define (enumerate-tree tree)
  (cond ((null? tree) `())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(provide enumerate-interval)
(define (enumerate-interval low high)
  (if (> low high)
      `()
      (append (list low)
              (enumerate-interval (+ low 1) high))))


(provide zip-my)
(define (zip-my seqs)
  (accumulate-n cons `() seqs))

(provide list-op)
(define (list-op op seq)
  (accumulate op (car seq) (cdr seq)))

(provide map-deep)
(define (map-deep op . z)
  (map (lambda (x)
         (list-op op x))
       (zip-my z)))

(provide flatmap)
(define (flatmap proc seq)
  (accumulate append `() (map proc seq)))

(provide prime?)
(define (prime? x)
  (define (iter y)
    (cond ((>= y x) #t)
          ((= (remainder x y) 0) #f)
          (else (iter (+ y 1)))))
  (iter 2))
