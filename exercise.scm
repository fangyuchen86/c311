(load "pmatch.scm")

(define empty-k 
  (lambda ()
    (let ([okay #t])
      (lambda (v)
        (if okay
            (begin
              (set! okay #f)
              v)
            (errorf 'empty-k "k invoked in non-tail position"))))))


(define length
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [else (length (cdr ls) (lambda (a) (k (add1 a))))]
      )))

(define binary-to-decimal
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else
       (binary-to-decimal (cdr n) (lambda (a)
                                    (k (+ (car n) (* 2 a))))
                          )]
      )))