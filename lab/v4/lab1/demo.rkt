#lang racket

(define x 10)
(define y (+ x 100))

(+ x y)
(quotient x y)
(/ x y)

; null == '()
(append '() '(1 2 3))
(cons '() '(1 2 3))
(cons '() '(1 2 "ana" /))

(define (sum L)
  (if (null? L)
      0
      (+ (car L) (sum (cdr L)))))
(sum '(1 2 3))

(define (f x y z)(x y z))
(f cons 1 null)

(list 1 2 10)
(cons 10 '(2))
(reverse '(1 2 3))

(define (rev L)
  (cond
    ((null? L) null)
    (else (append (rev (cdr L)) (list (car L))))
  ))

; '(1 2 3) -> '(3 2 1)
(rev '(1 2 3))
