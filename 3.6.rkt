#lang racket
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define (rand2 m)
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new)
      (set! x new)
      x)
    (define (dispatch m)
      (cond ((eq? m `generate) generate)
            ((eq? m `reset) reset)
            (else
             (error "Unkonw request --RAND2"))))
    dispatch))