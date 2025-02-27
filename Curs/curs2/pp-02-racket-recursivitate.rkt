#lang racket


; secvențiere expresii

(define a 510)

(define (fn x)
  ; block

  (define a 5)
  
  ; expresia 1
  (if (> x 2)
      'mai-mare-ca-2
      'nu-mai-mare-ca-2
      )

  ; expresia 1a
  (display "Hello there")(newline)
  ; efect lateral de afișare la consolă

  (display a)
  
  ; expresia 2
  (if (< x 3)
      'mai-mic-ca-3
      'nu-mai-mic-ca-3
      )
  ) ; lambda va întoarce la aplicare
; valoarea întoarsă de ultima expresie
; din corp


; Recursivitate pe coadă și pe stivă


(define (F n)
  (if (zero? n)
      1
      (* n (F (sub1 n)))
      ))

(F 5)


(define (mul a b)
  (if (zero? a)
      0
      (+ b (mul (- a 1) b))))

(define (F2a n r) ; r - rezultatul parțial pentru toate apelurile de până acum (calculat până la intrarea în acest apel)
  (if (zero? n)
      r
      (F2a (sub1 n) (mul r n))
      ))

(define (F2 n)
  (F2a n 1)
  )

(F2 5)



(define (sumList L)
  (if (null? L)
      0
      (+ ; adun
       (car L) ; primul element
       (sumList (cdr L)) ; suma restului elementelor
       )
      ))

; exemplu:
(sumList '(1 2 3 4 5))



(define (sumListTailAux L sum)
  (if (null? L)
      sum ; am deja suma
      (sumListTailAux
       (cdr L)
       (+ sum (car L)))
      ))

(define (sumListTail L)
  (sumListTailAux L 0)
  )

(sumListTail '(1 2 3 4 5))



; if cu evaluare normală (leneșă)

(if #t (display "True") (display "False"))
(newline)

; if aplicativ (care își evaluează argumentele)

(define (myIF cond valT valF)
  (if cond valT valF))

(myIF #t (display "True") (display "False"))
(newline)














