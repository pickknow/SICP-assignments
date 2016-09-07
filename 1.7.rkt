(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))


;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x)
;                 x)))
;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))


(define (sqrt-iter guess x)
  (define (iter guess last)
    (if (good-enough? guess last)
      guess
      (iter (improve guess x)
                 guess
                 )))
  (iter guess 2))
(define (good-enough? guess last)
  (< (/ (abs (- guess last)) last) 0.01))

(sqrt-iter 1.0 400)