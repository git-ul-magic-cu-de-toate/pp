#lang racket

(define (count-smaller L)
  (let* [(count (λ (x) ; functie care numara cate cate elemente din lista noastra sunt mai mici
                  (foldr (λ (p acc) ; decat un numar primit ca parametru
                           (if (< p x) ; daca numarul curent e mai mic decat x
                               (add1 acc) ; incrementez acumulatorul
                               acc ; altfel il intorc fara a il mai modifica
                               ))
                           0 L))
                  )
         (remove-dups (foldr (λ (p acc) ; lista initiala fara duplicate
                               (if (member p acc) ; daca elementul curent din lista se gaseste deja in
                                   acc ; acc, atunci nu fac modificari
                                   (cons p acc) ; altfel il adaug in acumulator.
                                   ))
                             '() L)
                      )]
    (foldr (λ (p acc) (cons (cons p (count p)) acc)) '() remove-dups))) ; la final construiesc lista
                                                                        ; in care fiecare element are
(count-smaller '(1 2 3 2 4 3 1 2))                                      ; asociat nr-ul corespunzator.