(define (force delayed-object)
  (delayed-object))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run>)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (steam-map1 proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)
                         (stream-map1 proc (stream-cdr s))))))

(define (stream-map proc . agrstreams)
  (if (stream-null? (car agrstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map cdr argstreams))))))
                    