#lang planet neil/sicp
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (make-table . table-name) 
    (if (null? table-name)
        (list '*table*)
        table-name))
(define (lookup keys table) 
  (let ((record (assoc (car keys) (cdr table))))          
    (if record
        (cond ((null? (cdr keys))                    
               (cdr record))
              (else
               (lookup (cdr keys)  record)))
        #f)))

(define (insert! keys value table)
  (let ((current-key (car keys))
        (remain-key (cdr keys)))
    (let ((record (assoc current-key (cdr table))))
      (cond ((and record (null? (cdr remain-key)))
             (set-cdr! record value)
             table)
            ((and record (not (null? remain-key)))
             (insert! remain-key value record)
             table)
            ((and (not record) (not (null? remain-key)))
             (join-in-table (insert! remain-key value (make-table current-key)) table) 
             table)
            ((and (not record) (null? remain-key))                 
             (set-cdr! table
                       (cons (cons current-key value) (cdr table)))
             table)))))
    
(define (addjion-table key value)
  (let ((table (make-table key)))
    (insert!  (list key) value table)
    ))

 (define (join-in-table new-record table)
    (set-cdr! table
              (cons new-record (cdr table))))

;(define a (make-table))
;(insert! `(a b c) 1 a)
;(lookup `(a b c) a)        
        
