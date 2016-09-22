;recursive
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))
(define (indentity x) x)
(define (inc x) (+ x 1))
;(product-recursive indentity 3 inc 4)
(define (factorial n)
  (product-recursive indentity 1 inc n))
;(factorial 5)
;iterator
(define (product-iterator term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
;(product-iterator indentity 3 inc 4)
;b
(define (divide a)
  (/ (* a (+ a 2))
     (* (+ a 1) (+ a 1))))
(define (pi n)
  (define (next a)
    (+ a 2))
  (* 4.0 (product-recursive divide 2 next n)))
(pi 100)
;3.1570
