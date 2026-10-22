#lang racket

(provide (all-defined-out))

; TODO 1
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți recursivitate pe stivă.

(define (get-men mpref)
    (cond
    ((null? mpref) '())  ;verifică dacă lista de preferințe a bărbaților este goală
    (else
      (cons (caar mpref)
            (get-men (cdr mpref)))))) ;primul element din prima pereche din lista de preferințe + apelul recursiv al funcției get-men pe restul listei de preferințe a bărbaților

; TODO 2
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți recursivitate pe coadă.

(define (get-women wpref)
  (get-women-queue wpref '()))

(define (get-women-queue wpref list-women)
  (if (null? wpref)
       list-women ;lista de preferințe este vidă => coada este returnată
      (get-women-queue (cdr wpref) (append list-women (list (caar wpref))))) ;numele primei femei din lista de preferințe + se adaugă la coada curentă
  )

; TODO 3
; Implementați o funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Observație: de fiecare dată când ne referim la lista
; preferințelor unei persoane p, ne referim la o listă care conține
; doar persoanele de sex opus, nu și pe p pe prima poziție.

(define (get-pref-list pref person)
  (if (null? pref)   ;caz de baza, verif lista vida
        '()
       (if (equal? person (car (car pref)))
               (cdr (car pref))
               (get-pref-list (cdr pref) person)
               )))


; TODO 4
; Implementați o funcție recursivă care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosiți operatori condiționali, folosiți în schimb operatori
; logici pentru a obține același efect.

(define (preferable? pref-list x y)
  (or (and (not (null? pref-list))
           (equal? (car pref-list) x))
      (and (not (null? pref-list))
           (not (equal? (car pref-list) y))
           (preferable? (cdr pref-list) x y))))

  ;(cond
    ;((null? pref-list) #f)
    ;((equal? (car pref-list) x) #t) 
    ;((equal? (car pref-list) y) #f) 
    ;(else (preferable? (cdr pref-list) x y))))

; TODO 5
; Implementați o funcție recursivă care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți cond.

(define (get-partner engagements person)
  (cond
    ((null? engagements) #f)   
    ((equal? person (caar engagements))   ;cazul în care persoana este pe prima poziție într-o logodnă
                    (cdar engagements))
    (else (get-partner (cdr engagements) person)))) ;continuăm să parcurgem lista de logodne


; TODO 6
; Implementați o funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - lista logodnelor este completă, este un posibil rezultat al problemei
; - logodnele din listă au pe prima poziție persoana de același gen cu p2
; - un partener p' este mai potrivit decât p2 dacă îndeplinește 2 condiții:
;   - p1 îl preferă pe p' în raport cu p2
;   - p' îl preferă pe p1 în raport cu persoana cu care este logodit


(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (define list1 (get-pref-list pref2 (car p1-list)))
  (define thatpartner (get-partner engagements (car p1-list)))
    (cond
    ((null? p1-list) #f)
    ((preferable? p1-list p2 (car p1-list)) #f)
    ((preferable? list1 p1 thatpartner) #t)
    (else (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements))
    ))