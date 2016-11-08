#lang sicp
(#%require sicp-pict)
(#%require "lib/painter.rkt")
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-vect painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1)
                     (make-vect 0 0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.5)))
    (let ((paint-left
           (transform-painter painter1 (make-vect 0 0)
                      split-point
                      (make-vect 0 1)))
          (paint-right
           (transform-painter painter2 split-point
                      (make-vect 1 0)
                      (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
(define (flip-horzi painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.5 0.5)))
    (let ((below-paint (transform-painter painter1 (make-vect 0 0)
                                          (make-vect 1 1)
                                          split-point))
          (top-paint (transform-painter painter2 split-point
                                        (make-vect 1 1)
                                        (make-vect 0 1))))
      (lambda (frame)
        (below-paint frame)
        (top-paint frame)))))
(define (below2 painter1 painter2)
  (rotate90 (beside painter2 painter2)))