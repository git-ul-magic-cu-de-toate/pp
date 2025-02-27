#lang racket

(define (greater-sum L)
  (filter (λ (S)            ; extrag din lista L acele liste pentru care
            (>=
             (foldr + 0 S)  ; suma e mai mare 
             (foldr * 1 S)  ; decat produsul
             ))
          L))

(greater-sum '((1 2 3) (4 5) (1 2) (0.5 0.5)))