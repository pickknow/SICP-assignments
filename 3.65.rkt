#lang racket
(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (display-stream s)
  (stream-for-each displayln s))
(define (zip-map proc . agrstreams)
  (if (null? (car agrstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car agrstreams))
       (apply zip-map
              (cons proc (map stream-cdr agrstreams))))))
(define (add-streams s1 s2)
  (zip-map + s1 s2))
(define (mul-streams s1 s2)
  (zip-map * s1 s2))
(define (div-streams s1 s2)
  (zip-map / s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (stream-cons (- s2 (/ (sqr (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
(define (partial-sums s)
  (stream-cons (stream-car s)
               (add-streams  (stream-cdr s)
                             (partial-sums s))))
(define (pi-summands n)
  (stream-cons (/ 1 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (ln2-seq n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-seq (+ n 1)))))
(define ln2
  (partial-sums (ln2-seq 1)))
(define ln2-tres (euler-transform (ln2-seq 1)))
(define ln2-tres-acc (accelerated-sequence euler-transform (ln2-seq 1)))

(stream-ref ln2-tres-acc 0)
(stream-ref ln2-tres-acc 1)
(stream-ref ln2-tres-acc 2)
(stream-ref ln2-tres-acc 3)
(stream-ref ln2-tres-acc 4)

        