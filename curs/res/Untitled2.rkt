#lang racket
;(define (my-split L x result)
 ; (cond [(null? L)
  ;       result]
   ;     [else
    ;     (let-values ([(start stop)
     ;                  (splitf-at L (lambda (i)
      ;                                (not (equal? x i))))])
(define (sum numbers) (foldr + 0 numbers))
(define (maxi L result)
  (if (null? L)
      result
      (if (> (car L) result)
          (maxi (cdr L) (car L))
          (maxi (cdr L) result))))
(define (3-sequence-max numbers separator)
  (define (cv numbers separator result)
    (cond
     [(null? numbers)
      result]
     [else
      (let-values
        ([(start stop)
          (splitf-at numbers (lambda (i) (not (equal? i separator))) )])    
        (cond
          [(null? stop)
           (append result (list start))]
          [else
            (cv (rest stop)
                      separator
                      (append result (list start)))]))]))
  (cv numbers separator null))

(define (hbrnm L)
(if (null? L)
    '()
    (append (list (sum (car L))) (hbrnm (cdr L)))))

(define (make-values start stop)
  (if (>= start stop)
      (list start)
      (append (make-values start (sub1 stop)) (list stop))))

(define (make-tail n result)
  (if (zero? n)
      result
      (make-tail (sub1 n) (append result (list (add1 (* (car (reverse result)) 3)))))))
(define (bfs n)
  (let* ((result (make-tail n '(1))))
    (dis result '(1))
   ))
(define (dis L result)
  (if (null? (cdr L))
      result
      (dis (cdr L)(append result (reverse (make-values (add1 (car L)) (car (cdr L)))) )))
)

(bfs 3)
           