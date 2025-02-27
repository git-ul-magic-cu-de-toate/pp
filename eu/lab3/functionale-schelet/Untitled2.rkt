#lang racket/gui
(define (ceva L p1 p2)
  (if (null? L)
      '()
      (if (equal? (list(car (car L))) (list p1))
          (cons (cons p1 p2) (ceva (cdr L) p1 p2))
          (cons (car L) (ceva (cdr L) p1 p2))
          )
      )
  )
(ceva '((1 . 2) (2 . 3) (3 . 4)) 2 6)
(cdr '((1. 2) (2 . 3) (3 . 4)))

(define (change-value lst target replacement)
  (cond
    ((null? lst) '())  ; Base case: empty list
    ((eq? (car lst) target)  ; If target found, replace it
     (cons replacement (change-value (cdr lst) target replacement)))
    (else  ; Otherwise, keep traversing the list
     (cons (car lst) (change-value (cdr lst) target replacement)))))
