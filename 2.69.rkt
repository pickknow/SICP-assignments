#lang racket
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge ls)
      (if (null? (cdr ls))
           ls
          (successive-merge (append
                             (list (make-code-tree (cadr ls)
                                                   (car ls)))
                             (cddr ls)))))

(define pairs `((B 2) (C 1) (D 1) (A 4)))
(generate-huffman-tree pairs)
`((leaf A 4) ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4) (A B C D) 8)
