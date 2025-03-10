#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")
(require srfi/41)

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur
(define (change-value x L)
  (if (member (car x) L)
      (cons #f (cdr x))
      x
      )) ;; schimba toate valorile egale cu arg dat ca parametru din perechile-elemente ale unei liste cu false

(define (make-list person engagements)
  (if (null? engagements)
      '()
      (append (make-list person (cdr engagements)) (list (cons (car (car engagements)) person)))
  )
  )
;; adauga o pesoana la o lista de logodne

(define (make-free-people person engagements)
  (if (null? engagements)
      (list person)
      (append (make-free-people person (cdr engagements)) (list (car (car engagements))))
      )
  )
;; face o lista cu persoanele nelogidite

(define (diff lst1 lst2)
  (filter (lambda (x) (not (member x lst2)))
          lst1))
;; face diferenta dintre doua liste

(define (a-new-list pref queue)
  (map (lambda (y) (filter (lambda (x) (not (member x queue))) y)) pref))
;; face lista cu persoanele care nu sunt in coada

(define (match person engagements pref1 pref2 queue)
 (let* ((ceva (let* ((preff1 (a-new-list pref1 queue)) (preff2 (a-new-list pref2 queue)))
  (engage (make-free-people person engagements) engagements preff1 preff2)))
        (difv (diff (get-couple-members engagements) (get-couple-members ceva))))
  ; (append ceva
  ; (list (cons #f (car (diff (get-couple-members engagements) (get-couple-members ceva))))))
   (if (null? difv)
       (append ceva (list (cons #f person)))
       (let loop ((divv difv) (cevva ceva))
       (if (null? divv)
           cevva
           (loop (cdr divv) (append cevva (list (cons #f (car divv)))))
           )
  )
  )
  )
  )
;; iau atat preferintele persoanelor feminine cat si masculine din coada, persoane nelogodite
;; si cate o persoana din lista de persoane libere cu cate o persoana din preferinta sa
;; apoi fac dieferenta dintre logodnele facute de mine si cele initiale.
;; daca diferenta e nula, atunci se adauga la logodnele facute de mine perechea formata din persoana pe care am
;; tot incercat sa o logodesc cu preferintele sale, perechea
;; perechea dintre el si false, insemnand ca nu a gasit pe cineva potrivit
;; stiind ca engagements din etapa 3 face deja logodne optime

; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)

(define (modified person engagements pref1 pref2 queue)
 (let* ((ceva (let* ((preff1 (a-new-list pref1 queue)) (preff2 (a-new-list pref2 queue)))
  (engage (make-free-people person engagements) engagements preff1 preff2)))
        (difv (diff (get-couple-members engagements) (get-couple-members ceva))))
  ; (append ceva
  ; (list (cons #f (car (diff (get-couple-members engagements) (get-couple-members ceva))))))
   (if (null? difv)
       (append ceva (list (cons #f person)))
       (let loop ((divv difv) (cevva ceva))
       (if (null? divv)
           cevva
           (loop (cdr divv) (append cevva (list (cons #f (car divv)))))
           )
  )
  )
  )
  )
;; aici e pe ideea functiei anterioare
;; dar cum acolo perechea incepea cu un barbat si aici cu o femeie,
;; aici trebuie sa inversez perechile din lista de logodne si sa apelez functia anterioara,
;; sau macar sa procedez la fel ca la cea anterioara

(define (is-woman person wpref)
  (member person (get-women wpref))
  ) ;; verifica daca aceasta persoana e femeie
;; persoana person tre sa fie de gen opus cu cea din coada

(define (change-false value L)
  (if (null? L)
      '()
      (if (not (car (car L)))
          (append (change-false value (cdr L)) (list (cons value (cdr (car L)))))
          (if (not (cdr (car L)))
              (append (change-false value (cdr L)) (list (cons (cdr (car L)) value)))
              (append (change-false value (cdr L)) (car L))
              )
         )      )
  )
;; daca primul element al perechii din lista de perechi e fals sau al doilea din pereche e fals,
;; atunci se inlocuieste cu valoarea data ca parametru
;; asta ajuta pentru a calcula progresiv ce ar putea da fals
;; ba din apelul cand in coada am femeie, ba cand in coada am barbat
;; ca in cazul in care am in coada femeie, trebuie sa fac switch la perechi
;; si sa fac ceva asemanator ca prima functie


(define (path-to-stabilityy engagements mpref wpref queue)
  ;; person trebuie sa fie de gen opus cu persoana din coada
  ;; prima persoana dintr-o pereche trebuie sa fie de gen opus cu cea din coada
  ;; aici toate logodnele sunt formate din femeie + barbat => coada = barbat ok
  ;; dar coada => femeie -> reverse
  ;; fct ajutatoare de la 1, doar ca am modif mpref cu wpref si inversare sit
  ;; in fct de baza am verif daca pesroana din coada e fem => modified, altfel pe 1
  ;; daca intra un barbat -> ok
  ;; daca intra femeie -> neok
  ;; fct ajutatoare -> am copiat de la 1 in care am modif mpref cu wpref daca era inversata sit
  ;; verif cum e persoana din coada: femeie -> fct noua. barbat -> 1.
  
  ; ia primul element si face match de primul element
  ; apoi, unde e #f pune pe elementele din coada in ordine

  (if (is-woman (car queue) wpref)
       (match (car queue) (deep-rev engagements) wpref mpref (cdr queue))
       (match (car queue) engagements mpref wpref (cdr queue))
  )
  )


(define (path-to-stability engagements mpref wpref queue)
  ;; person trebuie sa fie de gen opus cu persoana din coada
  ;; prima persoana dintr-o pereche trebuie sa fie de gen opus cu cea din coada
  ;; aici toate logodnele sunt formate din femeie + barbat => coada = barbat ok
  ;; dar coada => femeie -> reverse
  ;; fct ajutatoare de la 1, doar ca am modif mpref cu wpref si inversare sit
  ;; in fct de baza am verif daca pesroana din coada e fem => modified, altfel pe 1
  ;; daca intra un barbat -> ok
  ;; daca intra femeie -> neok
  ;; fct ajutatoare -> am copiat de la 1 in care am modif mpref cu wpref daca era inversata sit
  ;; verif cum e persoana din coada: femeie -> fct noua. barbat -> 1.
  
  ; ia primul element si face match de primul element
  ; apoi, unde e #f pune pe elementele din coada in ordine
  (displayln queue)
  (let* ((first-try (if (member (car queue) (get-women wpref)) (deep-rev (match (car queue) (deep-rev engagements) wpref mpref queue))
                       (match (car queue) engagements mpref wpref queue)))
        (chiu (cdr queue))
        (filtrat (filter (lambda (x) (if (not (and (car x) (cdr x))) #t #f)) first-try))
        (possibilities (change-false (cadr queue) filtrat))                               
        )
    (display possibilities)
    filtrat
    ))
 
; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (update-stable-match engagements mpref wpref)
  'your-code-here)


; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.
(define (build-stable-matches-stream pref-stream)
  (cond ((stream-empty? pref-stream) empty-stream)
        ((stream-pair? pref-stream)
         (stream-cons (gale-shapley (car (stream-car pref-stream)) (cdr (stream-car pref-stream)))
                      (build-stable-matches-stream (stream-cdr pref-stream))))
        (else empty-stream))) 