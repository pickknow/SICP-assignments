#lang racket
(define (conner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (rotate90 top-left))
              (conner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right conner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (filp-horiz quarter) quarter))
      (below (flip-vert half) half))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine (corner-split painter n))))
(defein (square-out painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180 )))
    (combine (corner-split painter n)))) 