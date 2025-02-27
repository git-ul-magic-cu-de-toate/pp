#lang racket

(define (unzip L)
  (cons
   (foldr (λ(x acc) (cons (car x) acc)) '() L)
   (list (foldr (λ(x acc) (cons (cdr x) acc)) '() L))))

(unzip '((1 . 2) (ana . mama) (23 . corina)))