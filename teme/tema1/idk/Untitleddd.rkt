#lang racket
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


(define (testt engagements mpref wpref)
  (filter (lambda (x) (if (cdr x) #t #f))
(let cvee ((ll (remove-duplicates 
(let hbrnm ((lst 
                  (let helper ((engagements engagements)
             (mpref mpref)
             (wpref wpref))
                    (if (null? engagements)
                        '()
                    (let* ((pereche (car engagements))
                           (femeie (car pereche))
                           (barbat (cdr pereche))
                           (restul (cdr engagements))
                           (invers (deep-rev engagements))
                           (rst (cdr invers))
                           (per (car invers))
                           (brb (car per))
                           (fem (cdr per))
                           )
 (if(null? engagements) '()
        (if (better-match-exists? barbat femeie
                               (get-pref-list mpref barbat) wpref engagements)
                    (append (helper restul mpref wpref) (list femeie barbat))
         (if (null? engagements)
             (helper invers wpref mpref)
             (if (better-match-exists? fem brb
                               (get-pref-list wpref fem) mpref rst)
                 
                    (append (helper restul mpref wpref) (list femeie barbat) )
                 (helper restul mpref wpref)
                 )
           )
         ))))))
            (wpref wpref))
  ;; end context hbrnm
  (if (null? lst)
      '()
      (if (pair? (car lst))
          (hbrnm (cdr lst) wpref)
          (append (hbrnm (cdr lst) wpref) (list (car lst)) ))
      ;; end else-if
      )
  ;; end if
  )))
           (engagements engagements))
  ;; end context cvee
  (if (null? ll)
      '()
      (append (cvee (cdr ll) engagements) (list (cons (car ll) (get-partner engagements (car ll)))) )))
  ;; end big let (cvee)
  ))

;; ---------------------------------------------------------------------------------------------------------------------
(define M
  '([adi ana bia cora]
    [bobo cora ana bia]
    [cos cora bia ana]))
(define W
  '([ana bobo adi cos]
    [bia adi cos bobo]
    [cora bobo cos adi]))
(define L '(((ana . adi) (bia . cos) (cora . bobo))
                                 ((ana . cos) (bia . adi) (cora . bobo)))
  )
(testt L M W)