#lang racket
(require math/base)
(define random-init 10)
(define rand
  (let ((x random-init))
    (define (generate)
      (set! x (random-natural x))
      x)
    (define (reset new)
      (set! x new)
      x)
    (define (dispatch m)
      (cond ((eq? m `generate) (generate))
            ((eq? m `reset) reset)
            (else
             (error "Unkonw request --RAND2"))))
    dispatch))

 (rand `generate)
 (rand `generate)
 (rand `generate)
 (rand `generate)
 ((rand `reset) 100)
 
 (rand `generate)
 (rand `generate)
