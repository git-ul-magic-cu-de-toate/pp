;; !!! Limbajul trebuie să fie Pretty Big !!!

;---------------------- Efecte laterale
(define counter 1)

(define (display-and-inc-counter)
  (display counter)
  (set! counter (add1 counter))) ; nu folosiți set! în programele voastre!

;(display-and-inc-counter)
;(newline)
;(display-and-inc-counter)

;; Exemplu despre cum este posibil și util ca un n definit mai jos să fie vizibil mai sus:
;(define a (λ () n))
;(define n 5)
;(a)


;---------------------- Legare dinamică
;(define (f)           
;  (g 5))
;
;(define (g x)        
;  (* x x))
;;(f)                  
;
;(define (g x)
;  (* x x x))
;;(f)                  

;---------------------- lambda
(define test
  (λ (x y z)
    (display x)
    (if (< y 4)
        (test (add1 y) z z)) ; if fără else este posibil în limbajul Pretty Big
    z))

;(test 1 2 4)
; => (test 1 2 4) => afișează 1 => apelează (test 3 4 4) => afișează 3 => (test 3 4 4) returnează 4 în neant =>(test 1 2 4) returnează 4

;---------------------- let-uri
(define x 2) ; să se încerce și cu linia asta comentată
;(let ((x 5) (y (add1 x))) 
;  (cons x y))
;
;(let* ((x (add1 x)) (y (add1 x)) (z (+ x y)))
;  (list x y z))
;
;(letrec ((par? (λ (n) 
;                 (if (zero? n)
;                     #t
;                     (impar? (sub1 n)))))
;         (impar? (λ (n)
;                   (if (zero? n)
;                       #f
;                       (par? (sub1 n))))))
;  (par? 242))

;(letrec ((x y) (y 1)) x)

;; Named let oferă și posibilitatea de a defini
;; și utiliza ad-hoc funcții helper
(define (fact-it n)
  (let loop ((n n) (acc 1))
    (if (zero? n)
        acc
        (loop (sub1 n) (* n acc)))))

;(map fact-it '(1 2 3 4 5))

;; Exercițiu: să inversăm un număr cu named let
(define (inversare nr)
  (let loop ((n nr) (inv 0))
    (if (zero? n)
        inv
        (loop (quotient n 10)
              (+ (* inv 10)
                 (modulo n 10))))))

;(inversare 5123)

; 5123 0 => 512 3(0*10+3) => 51 32(3*10+2) => 5 321(32*10+1) => 0 3215(321*10+5)

;---------------------- Funcții mutual recursive
(define par? (λ (n) 
                 (if (zero? n)
                     #t
                     (impar? (sub1 n)))))

(define impar? (λ (n) 
                 (if (zero? n)
                     #f
                     (par? (sub1 n)))))

;(impar? 5)

;---------------------- Context computațional
;(define a 1)
;(define (f x)
;  (+ x
;     (let ((x 5))
;       (* a x))))
;(f 2)
;(define a 2)
;(f 2)


;(define (fact n)
;  (if (zero? n)
;      1
;      (* n (fact (- n 1)))))
;(define g fact)
;(g 4)
;
;(define (fact n) n)
;(g 4)

(define (g x)
  (* x x))
(define f
  (g 5))

;f

(define (g x)
  (* x x x))
;f

;; Exercițiu tip examen cu ce am învățat până acum
;; Rezolvare two-sums
;; Se dă L cu nivel maxim de imbricare 1
;; S1 = suma elementelor care apar în lista mare
;; S2 = suma elementelor care apar în liste interioare
;; ex: '(1 3 5 (1 4) 5 (2 6 8) (3) 2)

;; După lab 2:
;; recursiv, cât mai eficient
(define (two-sums-2 L)
  (define (helper L S1 S2)
    (cond ((null? L) (cons S1 S2))
          ((list? (car L)) (helper (cdr L) S1 (+ (car (helper (car L) 0 0)) S2)))
          (else (helper (cdr L) (+ S1 (car L)) S2))))
  (helper L 0 0))

;(two-sums-2 '(1 3 5 (1 4) 5 (2 6 8) (3) 2))

;; După lab 3:
;; am învățat funcționale
(define (two-sums-3 L)
  (define (helper L S1 S2)
    (cond ((null? L) (cons S1 S2))
          ((list? (car L)) (helper (cdr L) S1 (foldl + S2 (car L))))
          (else (helper (cdr L) (+ S1 (car L)) S2))))
  (helper L 0 0))

;(two-sums-3 '(1 3 5 (1 4) 5 (2 6 8) (3) 2))

;; Fără recursivitate
(define (two-sums-3b L)
  ;; reprezentăm acc ca pe o pereche între S1 și S2
  (foldl (λ (x acc) (if (list? x)
                        (cons (car acc)
                              (foldl + (cdr acc) x))
                        (cons (+ (car acc) x)
                              (cdr acc))))
         '(0 . 0)
         L))

;(two-sums-3b '(1 3 5 (1 4) 5 (2 6 8) (3) 2))

;; După lab 4
;; am învățat let
(define (two-sums-4 L)
  (let ((outer (filter (compose not list?) L))
        (inner (apply append (filter list? L))))
    (cons (apply + outer) (apply + inner))))

(two-sums-4 '(1 3 5 (1 4) 5 (2 6 8) (3) 2))
                        
                     


