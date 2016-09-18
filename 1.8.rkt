(define (cube-root guess x)
  (if (good-enough? guess x)
      guess
      (cube-root (improve guess x)
            x)))
(define (improve y x)
  (/ (+ (/ x (square y)) (* 2 y))
     3))
(define (good-enough? guess x)
  (< (abs (- x (cube guess))) 0.001))
(define (cube x) (* x x x))
(define (square x) (* x x))
(cube-root 1.0 27)
;3.000
