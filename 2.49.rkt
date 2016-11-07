#lang sicp
(#%require sicp-pict)
(#%require "lib/painter.rkt")


(define draw-frame-board
  (list (make-segment (make-vect 0 0)
                      (make-vect 0 0.99))                                 
        (make-segment (make-vect 0 0.99)
                      (make-vect 0.99 0.99))
        (make-segment (make-vect 0 0 )
                      (make-vect 0.99 0))
        (make-segment (make-vect 0.99 0 )
                      (make-vect 0.99 0.99))))
(define draw-frame-opposite
  (list (make-segment (make-vect 0 0)
                      (make-vect 1 1))
        (make-segment (make-vect 1 0)
                      (make-vect 0 1))))
(define draw-frame-rhombus
  (list (make-segment (make-vect 0.5 0)
                      (make-vect 1 0.5))                                 
        (make-segment (make-vect 1 0.5)
                      (make-vect 0.5 1))
        (make-segment (make-vect 0.5 1 )
                      (make-vect 0 0.5))
        (make-segment (make-vect 0 0.5)
                      (make-vect 0.5 0))))
(define wave 
  (list (make-segment (make-vect 0.3 0.99)
                      (make-vect 0.25 0.88))
        (make-segment (make-vect 0.25 0.88)
                      (make-vect 0.3 0.77))
        (make-segment (make-vect 0.3 0.77)
                      (make-vect 0.2 0.77))
        (make-segment (make-vect 0.2 0.77)
                      (make-vect 0 0.88))
                      ))
(paint (segments->painter wave))
 