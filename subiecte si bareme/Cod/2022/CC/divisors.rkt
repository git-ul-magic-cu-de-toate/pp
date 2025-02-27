#lang racket

(define (divisors L1 L2)
  (let ((find-divs (λ(x) ;consider o functie anonima
                     ;ce imi extrage toate elementele din L2 ce sunt divizori
                     ;ai lui "x".
                     (filter (λ(num) (= (modulo x num) 0)) L2)
                     )))
    (map (λ(y) (cons y (list (find-divs y)))) L1) ;adaug la fiecare element
    ;corespunzator din L1 lista formata cu divizorii sai din L2.
    ))

(divisors '(25 30 100) '(2 3 5))