
;; problem 1
;; list-ref
(define list-ref
  (lambda (ls n)
    (letrec
      ([nth-cdr (lambda (n)
                  (cond
                    [(zero? n) ls]
                    [else (cdr (nth-cdr (sub1 n)))]))]
       )
      (car (nth-cdr n)))))

;; problem 2
;; union

(define (union ls1 ls2)
  (letrec
    ([contains (lambda (e ls)
                 (cond
                   [(null? ls) #f]
                   [(eq? e (car ls)) #t]
                   [else (contains e (cdr ls))]))])
    (cond
      [(null? ls1) ls2]
      [(contains (car ls1) ls2) (union (cdr ls1) ls2)]
      [else (union (cdr ls1) (cons (car ls1) ls2))])
    ))

