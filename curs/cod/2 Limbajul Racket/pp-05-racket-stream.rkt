#lang racket
; Mihnea Muraru & Andrei Olaru

;(display "----- Operatori pe fluxuri -----\n")

(define-syntax-rule (pack expr)
  ;  (lambda () expr)) ; închideri
  (delay expr)) ; promisiuni

;(define unpack (λ (package) (package))) ; închideri
(define unpack force) ; promisiuni



; =====================================
(define-syntax-rule (stream-cons h t)
  ;; define-syntax-rule definește o construcție nestrictă,
  ;; deci apelul stream-cons nu va evalua h și t
  (cons h (pack t)))

(define stream-car car)

(define (stream-cdr s)
  (unpack (cdr s)))

(define stream-nil '())

(define stream-null? null?)




; ===============================

(define (stream-take s n)
  (cond ((zero? n) '())
        ((stream-null? s) '())
        (else (cons (stream-car s) (stream-take (stream-cdr s) (- n 1))))))

(define (stream-drop s n)
  (cond ((zero? n) s)
        ((stream-null? s) s)
        (else (stream-drop (stream-cdr s) (- n 1)))))

(define (stream-map f s)
  (if (stream-null? s) s
      (stream-cons (f (stream-car s))
                   (stream-map f (stream-cdr s)))))

(define (stream-filter f? s)
  (cond ((stream-null? s) s)
        ((f? (stream-car s)) (stream-cons (stream-car s) (stream-filter f? (stream-cdr s))))
        (else (stream-filter f? (stream-cdr s)))))

(define (stream-zip-with f s1 s2)
  (if (or (stream-null? s1) (stream-null? s2)) stream-nil
      (stream-cons (f (stream-car s1) (stream-car s2))
                   (stream-zip-with f (stream-cdr s1) (stream-cdr s2)))))

(define (stream-append s1 s2)
  (if (stream-null? s1) s2
      (stream-cons (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-assoc k s)
  (cond
    ((stream-null? s) #f)
    ((equal? (car (stream-car s)) k) (stream-car s))
    (else (stream-assoc k (stream-cdr s)))))

(define (list->stream L)
  (if (null? L) stream-nil
      (stream-cons (car L) (list->stream (cdr L)))))

; ===============================================
;(display "----- Definiri de fluxuri -----\n")

;(display "ones:\n")

; construim o listă infinită de numărul 1, care se numește ones
; (car ones) -> 1
; (car (cdr ones)) -> 1
; (car (cdr (cdr ones))) -> 1
; ....
; (car (cdr (cdr .... (cdr ones) ... ))) -> 1


;(define ones (cons 1 ones))
;(define (ones) (cons 1 (ones)))
;(define ones (cons 1 (λ () ones)))
;(define ones (cons 1 (delay ones)))

(define ones (stream-cons 1 ones))


;(display "naturals:\n")
(define (naturalsFrom start)
  (stream-cons start
               (naturalsFrom (add1 start))))
(define naturals (naturalsFrom 0))

(define naturals2 (let naturalsFrom ((start 0))
                    (stream-cons start (naturalsFrom (add1 start))))
  )

;(display "Powers of 2: ")
(define pow2 (let build ((start 1))
               (stream-cons start (build (* 2 start)))))

;(display "primes: ")
(define primes
  (let sieve ((numbers (naturalsFrom 2)))
    (stream-cons (stream-car numbers)
                 (sieve (stream-filter (λ (n) (not (zero? (remainder n (stream-car numbers)))))
                                       numbers))
                 )))

  
;(display "even naturals: ")



















; =============================================== LAZY BFS
;(display "----- Căutare leneşă în spaţiul stărilor -----\n")


(define symbols '(a b c)) ;; alfabetul pentru palindroame
(define expand-string ;; pe baza lui s, mai construiește o serie de șiruri, de lungime + 1
  (λ (s) (map (λ (symb) (cons symb s)) symbols)))
(define palindrome? (λ (s) (equal? s (reverse s))))
(define (n-palindrome? n) (λ (s) (and (equal? n (length s)) (palindrome? s)))) ; curry

