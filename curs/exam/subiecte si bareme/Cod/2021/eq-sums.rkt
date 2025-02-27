#lang racket

(define (eq-sums? L)
  (let* [(rev (reverse L))
         (sums (map (λ(x y) (+ x y)) L rev))]
    (andmap (λ(x) (equal? (car sums) x)) sums)))

(eq-sums? '(1 4 2 5 3 6))
(eq-sums? '(1 4 5 1 3 6))