#lang racket
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


;(if x (let ((y (calcul))) (* y (y + 1))) 0)


;; clasic
(define prod1
  (λ (x y) ; în y primesc o valoare
    (displayln "prod")
    (if x (* y (+ y 1)) 0)))

(define test1
  (λ (xarg)
    (let ((yarg 5))
      (prod1 xarg (and (display "y ") yarg))))
  ; în al doilea argument, prod primește valoarea 5
  )

(test1 #f) (test1 #t) (displayln "------ ^ direct")

; îmi doresc să obțin
; prod
; y prod

;; quote + eval
(define prod2
  (λ (x y) ; în y primesc o expresie quoted
    (displayln "prod")
    (if x (* (eval y ns) (+ (eval y ns) 1)) 0)))
; eval necesită un spațiu de nume în care să evalueze expresia
; aici, am dat spațiul de nume global; o soluție ar fi să transmitem și spațiul de nume local ca argument

(define test2
  (λ (xarg)
    (let ((yarg 5))
      ;(prod2 xarg (quote (and (display "y ") yarg)))
      ; în al doilea argument, prod primește expresia "(and (display "y ") y)", fără context
      ; astfel că "yarg" nu va fi legat la nimic în momentul lui eval
      
      ; alternativ, contruiesc, textual, expresia calculului,
      ; dar evaluez argumentele pe care altfel nu le văd în prod
      (prod2 xarg `(and (display "y ") ,yarg))
      ))
  )

(test2 #f) (test2 #t) (displayln "------ ^ quote & eval")

; (paranteză
; ' quote
; ` quasiquote
; , unquote
; )



;; inchidere λ
(define prod3
  (λ (x y); în y primesc o închidere
    (displayln "prod")
    (if x (* (y) (+ (y) 1)) 0)))

(define test3
  (λ (xarg)
    (let ((yarg 5))
      (prod3 xarg (λ () (and (display "y ") yarg)))))
  ; în al doilea argument, prod primește o închidere funcțională
  ; < (λ () (and (display "y ") y)) ; { yarg <- 5 } >
  )

(test3 #f) (test3 #t) (displayln "------ ^ închideri λ")


;; promisiuni
(define prod4
  (λ (x y) ; în y primesc o promisiune
    (displayln "prod")
    (if x (* (force y) (+ (force y) 1)) 0)))

(define test4
  (λ (xarg)
    (let ((yarg 5))
      (prod4 xarg (delay (begin (display "y ") yarg)))))
  ; în al doilea argument, prod primește o promisiune
  ;    < (and (display "y ") yarg) ; { yarg <- 5 } ; fără valoare calculată >
  ; după primul force, promisiunea devine
  ;    < (and (display "y ") yarg) ; { yarg <- 5 } ; valoare calculată: 5>
  ; la force-uri ulterioare pe această promisiune, se întoarce direct valoarea calculată
  )

(test4 #f) (test4 #t) (displayln "------ ^ promisiuni")

