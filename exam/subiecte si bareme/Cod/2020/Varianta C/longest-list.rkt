#lang racket

(define (longest-list L)
  (let* [(lists (filter list? L)) ; extrag fiecare lista din lista L
         (lengths (cons (length L) (map (λ(x) (length x)) lists)))] ; creez o lista cu lungimile
    (foldr (λ(x acc)                                                ; listelor anterioare gasite.
             (if (< acc x)
                 x
                 acc))
           0 lengths))) ; la final extrag maximul din lista formata anterior.

(longest-list '(0 1 2 (3 4 3 4 3 4 3 4 3 4) 5 (6) 7))
(longest-list '(0 1 2 (3 4 3 4 3 4) 5 (6) 7))