(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(define (stream-limit s t)
  (let ((sub stream-cdr ))
    (if (<= (- sub s) t)
        sub
        (stream-limit s t))))


        