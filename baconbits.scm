(define-syntax cons$
  (syntax-rules ()
    [(cons$ a d) (cons (box (lambda () a)) (box (lambda () d)))]))

(define cdr$
  (lambda (p)
    ((unbox (cdr p)))))

(define car$
  (lambda (p)
    (let ([b (car p)])
      (let ([v ((unbox b))])
        (begin
          (set-box! b (lambda () v))
          v)))))