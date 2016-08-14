(define (make-zero-crossings1 input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      last-value last-avpt))))
(define (smooth i)
  (cons-stream
   (/ (+ (stream-car i)
         (stream-cdr i)) 2)
   (smooth  i)))
(define (make-zero input-stream last-value)
    (cons-stream
   (sign-change-detector (smooth input-stream) last-value)
   (make-zero-crossings (stream-cdr smooth)
                        (stream-car smooth))))