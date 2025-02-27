#lang racket

(define (two-sums L)
  (let* [(lists (filter list? L)) ; extrag toate listele
         (elements (filter (λ(x) (not (list? x))) L))] ; extrag toate elementele diferite de liste
    (cons (foldr + 0 elements) ; fac "cons" intre suma elementelor care nu sunt liste
          (foldr (λ(x acc) (+ (foldr + 0 x) acc)) 0 lists)))) ; si suma elementelor din fiecare lista

(two-sums '(1 2 (3 4) 5 (6) 7))