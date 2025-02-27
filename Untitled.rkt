#lang racket/gui
(define mmatrix '((1 2 3) (4 5 6) (7 8 9)))
(define (transpose matrix)
(apply map list matrix))
; (apply map cons ((1 2 3) (4 5 6))) -> (1 . 4) (2 . 5) (3 . 6)
; ia fiecare element al primei liste si il combina cu omologul din cealalta lista
; list merge pe mai multe listel cons pe doua -> deci list ia fiecare element din fiecare lista
; si il combina cu omologul din celelate liste facnd o lista mai mare
(require racket/trace)
;(trace transpose)
;(transpose mmatrix)
(define M
  '([adi  ana  bia cora]
    [bobo cora ana bia ]
    [cos  cora bia ana ]))
(define W
  '([ana  bobo adi cos ]
    [bia  adi  cos bobo]
    [cora bobo cos adi ]))
(define x 'bobo)
 ;;(filter (lambda (x) (equal? x y)) (cadr M))
(if (equal? x (car (foldr (lambda (L acc) (cons (car L) acc)) '() M)))
 (cons (car(foldr (lambda (L acc) (cons (cdr L) acc)) '() M)) )
 (car(cdr(foldr (lambda (L acc) (cons (cdr L) acc)) '() M))))

