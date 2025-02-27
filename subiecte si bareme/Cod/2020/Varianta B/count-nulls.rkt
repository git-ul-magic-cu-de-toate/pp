#lang racket

(define (count-nulls L)
  (let* [(cnt1 (length (filter null? L))) ; numarul de liste vide pe primul nivel
         (inner (filter list? L)) ; listele vide de pe primul nivel
         (cnt2 (foldr (λ(x acc) (+ acc (length (filter null? x)))) 0 inner))] ; numarul de liste vide
    (+ cnt1 cnt2)))                                                           ; pe nivelul 2.

(count-nulls '(() 1 (2 () (3 4 5)) (())))