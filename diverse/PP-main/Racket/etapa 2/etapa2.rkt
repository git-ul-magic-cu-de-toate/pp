#lang racket

(provide (all-defined-out))

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; TODO 1
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți orice funcțională exceptând foldl/foldr.

(define (get-men mpref)
  (map car mpref))


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.

;valoare initiala pt acumulator este '()
(define (get-women wpref)
  ; se aplică funcția asupra fiecărui element din listă, pornind de la ultimul și ajungând la primul
  ; pref -> elementul curent
  ; newlist -> acumulator
  (foldr (lambda (pref newlist)
           ;adaugă numele femeii în fața listei acumulatorului
           (cons (car pref) newlist))
           '() wpref))


; TODO 3
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosiți minim o funcțională și minim o funcție anonimă.

;assoc -> functia care gaseste prima pereche (de tipul (x . y))
;               din lista pref care are pe prima poziție person
(define (get-pref-list pref person)
  (cdr (assoc person pref)))


; TODO 4
; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Implementați o funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosiți funcția member.

(define (preferable? pref-list x y)
   ; x și y apar ambele în lista de preferințe pref-list
   ; se verifică dacă x apare în pref-list
   (and (member x pref-list)
        ; se verifică dacă y apare în pref-list
        (or (not (member y pref-list)) 
            ; lenght = nr de pers din lista de dinainte
            ; lungimea listei care conține x este mai mare decât lungimea listei care conține y
           (> (length (member x pref-list)) 
              (length (member y pref-list))))))

; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.

; p -> predicat
; L -> lista

(define (find-first p L)
  (define (find-helper e newlist)
    (or newlist ; dacă newlist este deja bun -> nu se mai trateaza restul elementelor din listă
        (if (apply p (list e)) ; predicatul este adevărat pentru elementul e
            e ; devine noul acumulator
            #f)))
  (define rez (foldl find-helper #f L))
  (if (member rez L)  ; elementul returnat se găsește în lista inițială L
      rez
      #f))
;după ce a fost găsit primul element care satisface predicatul, se oprește parcurgerea listei

;map => aplica predicatul pe fiecare element din lista
;fold  => pastreaza primul element care satisface predicatul
;member  => verifica dacă elementul s-a găsit

; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).


;gaseste logodna persoanei primite ca argument și să întoarcă partenerul acesteia
;                                (dacă persoana este prima în perechea de logodnă)

(define (get-partner engagements person)
  ;verifică dacă person se află pe prima poziție a logodnei
  (define (get-partner-helper x)
    (eq? person (car x)))
  ; până când găsește primul element care să satisfacă condiția din funcția helper
  ; dacă se găsește o astfel de pereche, aceasta este stocată în variabila partner
  (define pereche (find-first get-partner-helper engagements))
  ; verifică dacă pereche este o pereche (pair?)
  (if (and pereche (pair? pereche))
      ; returnează al doilea element al acesteia
      (cdr pereche)
      #f))

; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.


;val -> înlocuieste primul element din L care satisface predicatul p

(define (change-first p L val)
  (define (change-first-aux L)
    (cond ((null? L) L)
          ; primul element al listei L satisface predicatul p
          ; se întoarce o nouă listă care începe cu val + restul listei L
          ((apply p (list (car L)))
           (cons val (cdr L)))
          (else
           ; primul element din lista L nu satisface predicatul p
           ; elementul se adaugă la începutul listei de rezultate
           (cons (car L) (change-first-aux (cdr L))))))
  (change-first-aux L))


; TODO 8
; Implementați funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosiți change-first.

; p -> verifică dacă primul element al unei perechi conținute în lista engagements este egal cu p1

(define (update-engagements engagements p1 p2)
  ;se înlocuieste primul element din lista engagements care are prima componentă egală
  ;      cu p1 cu noul cuplu format din vechiul partener al lui p1 și noul partener p2
  (change-first (lambda (p)
                  (equal? (car p) p1))
                   engagements
                ; adaugă p2 ca noul partener pentru persoana p1, înlocuind vechiul partener
                (cons (car (assoc p1 engagements)) p2)))

; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.

(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (define list1 (get-pref-list pref2 (car p1-list)))
  (define thatpartner (get-partner engagements (car p1-list)))
    (cond
    ((null? p1-list) #f)
    ((preferable? p1-list p2 (car p1-list)) #f)
    ((preferable? list1 p1 thatpartner) #t)
    (else (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements))
    ))

; TODO 9
; Implementați funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie

(define (stable-match? engagements mpref wpref)
  (cond ((null? engagements) #t)
        (else (let ((woman (car (car engagements)))
                    (man (get-partner engagements (car (car engagements)))))
                (and (better-match-exists? woman man (get-pref-list wpref woman) mpref  (rest engagements))
                     (better-match-exists? man woman (get-pref-list mpref man) wpref (rest engagements)))))))
                
                   