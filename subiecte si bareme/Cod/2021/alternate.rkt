#lang racket

(define (alternate L f1 f2)
  (let* [(functions (flatten (make-list (length L) (list f1 f2))))
         (fs (take functions (length L) ))]
    (map (λ(x y) (y x)) L fs)))

(alternate '(1 2 3 4 5 6) add1 sub1)