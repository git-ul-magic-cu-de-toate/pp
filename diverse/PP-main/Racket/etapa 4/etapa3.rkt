#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.


; engagements -> listă de perechi de forma (femeie . bărbat) care reprezintă cuplurile existente

(define (get-unstable-couples engagements mpref wpref)
  ;se filtreaza toate cuplurile care nu sunt stabile
  ;cuplul este instabil -> inclus în lista rezultat
  (filter (lambda (x)
            (let*
                 ;se extrage bărbatul și femeia din cuplul curent
                ((man (car x))
                 (woman (cdr x))
                 ;listele preferințelor bărbatului și femeii 
                 (man-pref-list (get-pref-list mpref (cdr x)))
                 (woman-pref-list (get-pref-list wpref (car x)))
                 ;the-reversed-list care conține cuplurile din engagements inversate -> (bărbat . femeie)
                 ;motiv reverse : femeile nu sunt declarate ca fiind preferate de anumiți bărbați
                 (the-reversed-list (let reversed ((L engagements))
                                        (cond ((null? L)
                                           '())
                                           (else (cons (cons (cdr (car L)) (car (car L)))
                                                 (reversed (cdr L)))))))
                 ;există un alt partener mai bun pentru bărbatul sau femeia din cuplul curent
                 (better-match-for-men (better-match-exists? woman man man-pref-list wpref engagements))
                 (better-match-for-women (better-match-exists? man woman woman-pref-list mpref the-reversed-list)))
              (cond (better-match-for-women #t) ;cuplul este instabil -> #t
                    (better-match-for-men #t)   ;cuplu instabil -> cel puțin unul dintre parteneri are un partener preferat mai bun decât cel actual
                    (else #f))                  ;cuplul este stabil -> #f
              ))
          ;lista cu cuplurile instabile
          engagements))



; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.


(define (engage free-men engagements mpref wpref)
  ;aux = listă de bărbați nelogodiți free-men + listă parțială de logodne engagements
 (let aux ((burlaci free-men) (logodna engagements))
   ;se verifică dacă lista de bărbați este goală
    (cond ((null? burlaci)
           logodna)
          (else
           ;aux2 = preferințele bărbatului de pe prima poziție a listei man-pref
           (let aux2((man-pref (if (get-pref-list mpref (car burlaci))
                                                (get-pref-list mpref (car burlaci))
                                                '())))
                   ;se verifică dacă lista de preferințe este goală
                   (cond ((null? man-pref)
                          ;eliminăm bărbatul din lista de bărbați nelogodiți
                          (aux (cdr burlaci) logodna))
                         (else
                          ;dacă lista de preferințe nu este goală
                          ;w -> prima femeie din lista de preferințe
                          ;m -> bărbatul de pe prima poziție a listei de bărbați nelogodiți
                          (let* ((w (car man-pref))
                                 (m (car burlaci)))
                                 ;femeia w este deja logodită în lista de logodne engagements
                                 (cond ((get-partner logodna w)
                                        ;verificăm preferințele femeii w
                                        ;dacă ea îl preferă pe m în locul partenerului său actual m -> actualizăm lista de logodne engagements
                                        ;se înlatura perechea (w, m') + se adauga perechea (w, m)
                                        ;bărbatul m' devine nelogodit -> se adăuga înapoi în lista de bărbați nelogodiți
                                        (if (preferable? (get-pref-list wpref w) m (get-partner logodna w))
                                            (aux (cons (get-partner logodna w)
                                                       (map (lambda (p)
                                                              (if (equal? p (get-partner logodna w)) m p))(cdr burlaci)))
                                                 (update-engagements logodna w m))
                                            ;femeia w nu îl preferă pe m în locul partenerului său actual m'
                                            ;se trece la următoarea femeie din lista de preferințe a bărbatului m
                                            (aux2 (cdr man-pref))))
                                       (else
                                        ;femeia w nu este logodită
                                        ;se adăugă perechea (w, m) în lista de logodne engagements
                                        (aux (cdr burlaci) ;lista de bărbați nelogodiți fără primul bărbat
                                                  ;lista de logodne actualizată
                                                  (cons (cons w m) logodna))))))))))))


  
; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.

(define (gale-shapley mpref wpref)
  (engage (get-men mpref) ;lista cu barbatii necasatoriti
          '()             ;lista cu toate logodnele -> vida
          mpref           ;lista preferinte pt fiecare barbat
          wpref)          ;lista preferinte pt fiecare femeie
  )


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.

;get-couple-members creează o listă de liste conținând toate elementele din perechi,
;                   apoi concatenează aceste liste într-o singură listă

(define (get-couple-members pair-list)
  (apply append (map
                 (lambda (pair)
                   (list (car pair)
                         (cdr pair)))
                 pair-list)))

