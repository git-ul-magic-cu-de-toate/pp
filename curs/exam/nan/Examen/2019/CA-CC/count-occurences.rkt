#lang racket

(define (count-occ L1 L2)
  (let ([count (λ (x)
                 (length (filter (λ (p) (equal? x p)) L2))
                 )]) ; functie ce determina numarul de aparitii ale unui numar in lista L2
    (map (λ (y) (cons y (count y))) L1) ; la final construiesc rezultatul adaugand la fiecare
    ))                                  ; element din lista L1, numarul sau de aparitii in L2.

(count-occ '(1 4 5 3) '(1 3 2 4 1 5 3 9))