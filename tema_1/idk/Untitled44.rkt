#lang racket
(define my-lst '((1 . 2) (3 . 4) (5 . 6)))
(define (combine my-list)
  ;; mai intai fara let-uri
  (if (null? my-list)
      '()
     (append (list(car (car my-list)))(append (list (cdr (car my-list)) ) (combine (cdr my-list))) ))
  )
;(combine my-lst)
(define (my-func ll)
  (let cevaa ((ll ll))
    (if (null? ll)
        '()
        (append (list(car (car ll)))(append (list (cdr (car ll)) ) (cevaa (cdr ll))) )
        )))
;(my-func my-lst)

(define (get-men mpref)
  (car (apply map list mpref)))

(define (get-women wpref)
  (foldr (lambda (L acc) (cons (car L) acc)) '() wpref))

(define (find-first p L)
  (if (null? L)
      #f
      (if (p (car L)) (car L)
          (find-first p (cdr L)))))

(define (get-partner engagements person)
  (define my_test (find-first (lambda (L)
                (and (equal? (list (car(list(car L)))) (list person))
                                  (cdr L)))engagements))
  (if my_test (cdr my_test) #f)
     )

(define (get-pref-list pref person)
  (ormap (lambda (L)
         (if (equal? (list person) (list(car L)))
             (cdr L)
             #f
             )) pref))

(define (preferable? pref-list x y)
  (ormap (lambda (L)
           (if (null? L)
               #f
               (and (equal? L x)
                   (and (member y (member x pref-list)) #t)
                   )
               ))
         pref-list))

(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (if (null? p1-list)
      #f
      (if (preferable? p1-list p2 (car p1-list))
          #f
          (if (not (preferable?
                    (get-pref-list pref2 (car p1-list)) (get-partner engagements (car p1-list)) p1))
              #t
              (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)
             )
          )
      )
  )

(define (deep-rev L)
  (if (null? L)
      '()
      (cons (cons (cdr (car L)) (car (car L)))
            (deep-rev (cdr L)))))

;; ------------------------------------------------------------------------------------------------------------------------

(define M
  '([adi ana bia cora]
    [bobo cora ana bia]
    [cos cora bia ana]))
(define W
  '([ana bobo adi cos]
    [bia adi cos bobo]
    [cora bobo cos adi]))
(define L '((ana . cos) (bia . adi) (cora . bobo)))

;(testt L M W)

(define (stable-match engagements mpref wpref)
  (define (exists? p L)
  (cond
    ((null? L) #f)
    ((p (first L)) #t)
    (else (exists? p (rest L))))) 
  (not (exists? (λ(x) (or (better-match-exists? (car x) (cdr x) (get-pref-list wpref (car x)) mpref engagements)
                        (better-match-exists? (cdr x) (car x) (get-pref-list mpref (cdr x)) wpref engagements))) engagements)))

(define (stable-matchh engagements mpref wpref)
  (define (exists? p L)
    (if (null? L)
        #f
        (if (p (car L))
            #t
            (exists? p (cdr L)))))
  (if (exists? (λ(x) (if (better-match-exists? (car x) (cdr x) (get-pref-list wpref (car x)) mpref engagements)
                            #t
                            (better-match-exists? (cdr x) (car x) (get-pref-list mpref (cdr x)) wpref engagements))) engagements)
      #f
      #t))


(define (stable-match? engagements mpref wpref)
  (define (exists? p L)
    (if (null? L)
        #f
        (if (p (car L))
            #t
            (exists? p (cdr L)))))
  
  (if (exists? (λ(x)
                 (if (better-match-exists? (car x) (cdr x) (get-pref-list wpref (car x)) mpref engagements)
                            #t
                            #f)) engagements)
      (stable-match? (cdr engagements) mpref wpref)
      (if (null? engagements)
          '()
      (append (list (cons (car (car engagements)) (cdr (car engagements)))) (stable-match? (cdr engagements) mpref wpref))
      )))


(stable-match? L M W)
