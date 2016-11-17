#lang racket
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

(define (ripple-carry-adder A B S C C-out)
  (define (iter a b s c)
    (if (and (null? a) (null? b) (null? s))
        `done
        (let ((CK (make-wire)))
          (set-signal! CK c)
          (full-adder (car a) (car b) (car s) CK C-out)
          (iter (cdr a) (cdr b) (cdr s) (get-signal C-out)))))
  (iter A B S (get-signal C)))

;half-adder = or + and + inver + and
;full = half-adder *2 + all
; n((or + inver ) * 2 + and*5)
