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
(define (same-key? eq key records)
  (cond ((null? records) false)
        ((eq? key (caar records)) (car records))
        (else (same-key? key (cdr records)))))
(define (make-table2 same-key?)
  (let ((local-table (list `*talbe*)))
    (define (lookup key-1 key-2)
      (let ((subtable (same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (same-key? key-2 (cdr subtable))))
              (if record
                  (car record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (same-key? key-2 (cdr subtable))))
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