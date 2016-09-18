(define (square x) (* x x))
(define (fib n)
  (define (fib-iter a b count)
    (if ( = count 0)
        b
        (fib-iter (+ a b) a
                  (- count 1))))
  (fib-iter 1 0 n))

(define (p) (/ (+ 1 (sqrt 5)) 2))
(define (h) (/ (- 1 (sqrt 5)) 2))
(define (N x y)
  (define (N-iter n count product)
    (if (= 1 count)
        (* n product)
        (N-iter n (- count 1) (* product n))))
  (N-iter x y 1))

(define (g n)
  (/ (- (N (p) n) (N (h) n)) (sqrt 5)))

(fib 5)
(g 5)
;this answer is wrong
