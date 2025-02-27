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

;; de aici incolo ai nevoie de o singura functie
;; named let

(define (get-unstable-couples engagements mpref wpref)
  (let ((helper (let other-helper (engagements mpref wpref result)
                     (if (null? engagements)
                         result
                         (if (better-match-exists? (cdr (car engagements)) (car (car engagements))
                                                   (get-pref-list mpref (cdr (car engagements))) wpref engagements)
                             (other-helper (cdr engagements) mpref wpref
                                     (append (list (car (car engagements)) (car (cdr engagements))) result))
                             (if (null? engagements)
                                 (other-helper (deep-rev engagements) wpref mpref result)
                                 (if (better-match-exists? (cdr (car (deep-rev engagements))) (car (car (deep-rev engagements)))
                                                           (get-pref-list wpref (cdr (car (deep-rev engagements)))) mpref (deep-rev engagements))
                                     (other-helper (cdr engagements) mpref wpref
                                             (append (list (car (car engagements)) (car (cdr engagements))) result))
                                     (other-helper (cdr engagements) mpref wpref result)))))))
           (hbrnm (lambda (lst wpref res)
                    (if (null? lst)
                        res
                        (if (pair? (car lst))
                            (hbrnm (cdr lst) wpref res)
                            (hbrnm (cdr lst) wpref (append res (list (car lst)) res))))))
           (cvee (lambda (ll engagements rez)
                    (if (null? ll)
                        rez
                        (cvee (cdr ll) engagements (append (list (cons (car ll) (get-partner engagements (car ll)))) rez))))))
    (cvee (remove-duplicates (hbrnm (helper engagements mpref wpref null) wpref '())) engagements '())))

(define M
  '([adi ana bia cora]
    [bobo cora ana bia]
    [cos cora bia ana]))
(define W
  '([ana bobo adi cos]
    [bia adi cos bobo]
    [cora bobo cos adi]))
(define L '((ana . cos) (bia . adi) (cora . bobo)))

(get-unstable-couples L M W)
