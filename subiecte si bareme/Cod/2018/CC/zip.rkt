#lang racket

(define (zip L1 L2)
  (map cons L1 L2))

(zip '(ana mama bia) '(1 3 5))