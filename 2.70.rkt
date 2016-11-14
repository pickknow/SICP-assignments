#lang racket
(require "lib/huffman.rkt")
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))                 
(define (successive-merge ls)
  (if (null? (cdr ls))
      (car ls)     
      (successive-merge (append
                         (list (make-code-tree (cadr ls)
                                               (car ls)))
                         (cddr ls)))))

(define pairs `((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))
(define tree (generate-huffman-tree pairs))
tree
(encode `(get a job) tree)
                    
                    
                    
         
               
      
      

          
