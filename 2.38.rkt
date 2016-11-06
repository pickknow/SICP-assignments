#lang racket
(require "lib/lib1.rkt")

(fold-right / 1 `(1 2 3))
(fold-left / 1 `(1 2 3))
(fold-right list `() (list 1 2 3))
(fold-left list `() (list 1 2 3))
