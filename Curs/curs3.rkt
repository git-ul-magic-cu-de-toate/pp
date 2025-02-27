#lang racket

;---------------------- Funcții ca valori evaluate la ele însele
(define a +)
;a
;(a 2 3 5)

;null?

(define fs (list +
                 (λ (x y) (- x y y))
                 ((λ (x) (λ (y) (- x y y))) 5) ; (λ (y) (- 5 y y))
                 5))
;fs
;((cadr fs) 20 8)
;((caddr fs) 1)

;---------------------- Funcții ca valori de retur
(define (f x)
  (if (< x 100) + -))

;(f 25)
;((f 120) 16 4)

(define (g x)
  (λ (y)          ; y = 
    (cons x y)))  ; x = 

;(g 2)   ; x=2 retur=λ(y) (cons 2 y)
;( (g 2) '(5 2) ) ; (λ(y) (cons 2 y) '(5 2)) = '(2 5 2)

;---------------------- curry / uncurry
(define (plus-curry x)

  (λ (y)          ; y =    
    (+ x y )))    ; x = 2

(define (plus-uncurry x y)
  (+ x y))
; identic cu (define plus-uncurry +)

; Cum adun 2+4?
; Ce se întâmplă la aplicare parțială?
;(plus-uncurry 2 4)
;(plus-uncurry 2)

;((plus-curry 2) 4)
;(plus-curry 2)

; Cum obțin (cu efort minim) inc din plus?
(define inc-curry (plus-curry 1))
;(inc-curry 5)
;(define (inc-uncurry x) (plus-uncurry 1 x))
;(inc-uncurry 5)

; Pt a incrementa toate valorile dintr-o listă
;(map (plus-curry 1) '(2 3 4))
;(map ((curry +) 1) '(2 3 4))
;(map (λ (x) (+ 1 x)) '(2 3 4))

;---------------------- Reutilizare de cod
; inseram primul element din lista in
; sortarea recursiva a restului listei
 
(define (ins-sort op)
  (λ (L)
    (if (null? L)
        L
        (insert (car L) ((ins-sort op) (cdr L)) op))))

(define (insert e L op)
  (if (or (null? L) (op e (car L)))
      (cons e L)
      (cons (car L) (insert e (cdr L) op))))

(define sort< (ins-sort <)); scriem minim posibil, ne folosim de forma curry 
; varianta "lungă": (define sort< (λ (L) ((ins-sort <) L)))
; în loc de f(L) = g(L), scriu f = g, ceea ce e mai succint
(define sort> (ins-sort >))
;sort<
;(sort< '(5 3 1 4))
;(sort> '(5 3 1 4))

; intoarce lista in care toate parele au fost transformate si imparele au ramas la fel
;(define (transform-evens f L) ; f = transformarea
;  (cond ((null? L) L)
;        ((even? (car L)) (cons (f (car L)) (transform-evens f (cdr L))))
;        (else (cons (car L) (transform-evens f (cdr L))))))
;      
;(transform-evens inc-curry '(1 2 3 5 6))
;  
;(define (transform-odds f L)
;  (cond ((null? L) L)
;        ((odd? (car L)) (cons (f (car L)) (transform-odds f (cdr L))))
;        (else (cons (car L) (transform-odds f (cdr L))))))
;(transform-odds inc-curry '(1 2 3 5 6))

; Să observăm șablonul comun și să abstractizăm funcția:
(define (transform-pred pred?)
  (λ (f L)
    (cond ((null? L) L)
          ((pred? (car L)) (cons (f (car L)) ((transform-pred pred?) f (cdr L))))
          (else (cons (car L) ((transform-pred pred?) f (cdr L)))))))
; Cum obținem cele 2 funcții din funcția mai generală?
(define transform-evens (transform-pred even?))
(define transform-odds (transform-pred odd?))
;(transform-evens inc-curry '(1 2 3 5 6))
;(transform-odds inc-curry '(1 2 3 5 6))

; sa sortez descrescator listele interioare
;((transform-pred list?) sort> '(1 3 (2 4) (4 5 2) 5)) 


;---------------------- Șabloane comune
(define (sum-series start stop f next)

  (define (iter i)
    (if (> i stop)
        0
        (+ (f i) (iter (next i)))))

  (iter start))

(define (sum-interval a b) 
  (if (> a b)                             
      0
      (+ a (sum-interval (add1 a) b))))
;(sum-interval 1 10)
(define (ex1 a b) (sum-series a b (λ (x) x) add1))
;(ex1 1 10)

; e = 1/0! + 1/1! + 1/2! + 1/3! + ...
(define (sum-e n)

  (define (iter i)
    (if (> i n)
        0
        (+ (/ 1. (factorial i)) (iter (add1 i)))))

  (iter 0))

(define (ex2 n) (sum-series 0 n (λ (x) (/ 1. (factorial x))) add1))
 
; de implementat ulterior cu funcționale
; (range x y) = (x  x+1  x+2  ..  y-1)
(define (factorial n)

  (define (fact-tail n acc)
    (if (zero? n)
        acc
        (fact-tail (sub1 n) (* n acc))))

  (fact-tail n 1))
;(map factorial (range 1 10))
;(sum-e 15)
;(ex2 10)

; π^2/8 = 1/1^2 + 1/3^2 + 1/5^2 + ...
(define (sum-pi2/8 n)

  (define (iter i)
    (if (> i n)
        0
        (+ (/ 1. (sqr i)) (iter (+ 2 i)))))

  (iter 1))
;(sqrt (* 8 (sum-pi2/8 711111)))
(define (ex3 n) (sum-series 1 n (λ (x) (/ 1. (sqr x))) ((curry +) 2)))
;(sqrt (* 8 (ex3 711111)))

;---------------------- Funcționale
;; Maximul elementelor dintr-o listă de numere
(define (max-list L)
  (if (null? L)
      -inf.0                              
      (max (car L) (max-list (cdr L)))))  
;(max-list '(5 2 1 4 11 5 7 5 7 3))

(define (max-list-f L)
  (foldr max -inf.0 L))
;(max-list-f '(5 2 1 4 11 5 7 5 7 3))

;; Extragerea inițialelor dintr-o listă de nume
(define (initials names)
  (if (null? names)
      '()
      (cons (substring (car names) 0 1) (initials (cdr names)))))
;(initials '("Winston" "Leonard" "Spencer" "Churchill"))

(define (initials-f names)
  (map (λ (x) (substring x 0 1)) names))
;(initials-f '("Winston" "Leonard" "Spencer" "Churchill"))

;; Extragerea pronumelor dintr-o listă de cuvinte
(define (pronouns words)
  (cond ((null? words) '())
        ((member (car words) '(I you he she we they)) (cons (car words) (pronouns (cdr words))))
        (else (pronouns (cdr words)))))

(define YS '(In the town where I was born
                Lived a man who sailed to sea
                And he told us of his life
                In the land of submarines
                So we sailed up to the sun
                'Til we found a sea of green
                And we lived beneath the waves
                In our yellow submarine))
;(pronouns YS)

(define (pronouns-f words)
  (filter (λ (x) (member x '(I you he she we they))) words))
;(pronouns-f YS)

;; Pătratele elementelor dintr-o listă de numere
(define (squares L)
  (if (null? L)
      L
      (cons (sqr (car L)) (squares (cdr L)))))
;(squares '(1 2 3))

(define (squares-f L)
  (map sqr L))
;(squares-f '(1 2 3))

;; Numărul de elemente impare dintr-o listă
(define (count-odds L)
  (if (null? L)
      0                                   
      (+ (if (odd? (car L)) 1 0) (count-odds (cdr L)))))
;(count-odds '(1 2 3))

(define (count-odds-f L)
  ;(length (filter odd? L)))
  (foldl (λ (x acc) (if (odd? x) (add1 acc) acc))
         0
         L))
;(count-odds-f '(1 2 3 5 5 6 7 9 11 13))

;; Extragerea numerelor unei liste care sunt mai mici decât o valoare dată
(define (smaller-than x L)
  (cond ((null? L) L)
        ((< (car L) x) (cons (car L) (smaller-than x (cdr L))))
        (else (smaller-than x (cdr L)))))
;(smaller-than 3 '(1 2 3))

(define (smaller-than-f x L)
  (filter ((curry >) x) L))
;(smaller-than-f 3 '(1 2 3 4 1 3 2))

;---------------------- Implementare map și filter folosind fold
(define (my-map f L)
  (foldr (λ (x acc) (cons (f x) acc))
         '()
         L))

;(my-map add1 '(1 2 3 4 5))

(define (my-filter p L)
  (foldr (λ (x acc) (if (p x) (cons x acc) acc))
         '()
         L))

;(my-filter odd? '(1 2 3 4 5))
  
;---------------------- TDA-uri
(define make-complex cons)
(define real car)
(define imag cdr)

(define (add-c C1 C2)
  (make-complex
   (+ (real C1) (real C2))
   (+ (imag C1) (imag C2))))

;(add-c (make-complex -1 2) (make-complex 4 1))

;Constructori
;        empty-bst : -> BST
;        make-bst : BST x Elem x BST -> BST
(define empty-bst '())
(define make-bst list)

;Operatori
;        left : BST -> BST
;        right : BST -> BST
;        key : BST -> Elem
;        bst-empty? : BST -> Bool
;        insert-bst : Elem x BST -> BST
;        list->bst : List -> BST
(define left first)
(define right third)
(define key second)
(define bst-empty? null?)
(define (insert-bst x bst)
  (cond ((bst-empty? bst) (make-bst empty-bst x empty-bst))
        ((> (key bst) x) (make-bst (insert-bst x (left bst))
                                   (key bst)
                                   (right bst)))
        (else (make-bst (left bst)
                        (key bst)
                        (insert-bst x (right bst))))))
(define (list->bst L)
  (foldl insert-bst empty-bst L))

(list->bst '(3 6 1 2 4 8))

;'((() 1 (() 2 ())) 3 ((() 4 ()) 6 (() 8 ())))

