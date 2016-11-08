#lang racket
(require "lib/jihe.rkt")
;union-set  并集
;intersection-set 交集
;element-of-set?
;adjoin-set 加入新集合
(define a `(1 3 5 7 9))
(define b `(1 2 3 6 8))
(intersection-set a b)
(union-set a b)
         
         