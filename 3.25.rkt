#lang racket
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (make-table)
  (let ((local-table (list `*talbe*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (car record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtalbe)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      `ok)
    (define (dispatch m)
      (cond ((eq? m `lookup-proc) lookup)
            ((eq? m `insert-proc) insert!)
            (else
             (error "Unkonw operation --TABLE" m))))
    dispatch))



(define (make-tables)
  (let ((local-table (list `*table*)))
    (define (lookup l)
      (define (recur keys table) 
        (let ((record (assoc (car keys) (cdr table))))
          (if record
              (if (null? (cdr keys))
                  (car record)
                  (recur (cdr keys)  record))
              false)))
      (recur l local-table))
    (define (insert! l value)
      (define (recur keys table)            
        (let ((record (assoc (car keys) (cdr table))))
          (if record
              (if (null? (cdr keys))
                  (set-cdr! record value)
                  (recur (cdr keys) record))
              (if (null? (cdr keys))
                  (set-cdr! table
                            (cons (cons (car keys) value)
                                  (cdr table)))                  
                  (set-cdr! table
                            (cons (list (car keys)
                                        (recur (cdr keys)
                                          (cons (cadr keys) `())))
                                  (cdr table)))))))
      (recur l local-table)
      `ok)
    (define (dispatch m)
      (cond ((eq? m `lookup-proc) lookup)
            ((eq? m `insert-proc) insert!)
            (else
             (error "Unkonw operation --TABLE" m))))
    dispatch))
    

 
                           
        
        