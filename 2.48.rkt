#lang racket
(define (segments->pointer segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
(define (make-segment v1 v2)
  (list v1 v2))
(define start-segment car)
(define end-segment card)