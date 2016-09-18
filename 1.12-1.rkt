(define (fa a count) 
  (cond ((>= a count)
         (f 0 count 1);display a line
         (newline)
         (fa a  (+ count 1)))));next line

(define (f a b p)
  (cond ((> a 0) (display p)))
  (cond ((< a b)
         (f (+ a 1) b (g a (- b 1))))))

(define (g a b)
  (if (or (= a 0) (= a b))
      1
      (+ (g (- a 1) (- b 1)) (g a (- b 1)))))
(define (h n)
  (fa n 0))
(h 6)

      
