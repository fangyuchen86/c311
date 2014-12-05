#|
Samuel Waggoner
srwaggon

CSCI C311
Dan Friedman

Assignment 6

I took the time to redo this assignment, since the first time through I wasn't feeling very
confident after its completion.

I've successfully CPS'd each of these procedures except for #9, unify, which works only
in very simple cases.  I'm not greatly disturbed by its lack of functionality.  It's probably
something I have locked away in notes that I'll just hit up office hours for.

Thanks.
|#

(define empty-k 
  (lambda ()
    (let ([okay #t])
      (lambda (v)
        (if okay
            (begin
              (set! okay #f)
              v)
            (errorf 'empty-k "k invoked in non-tail position"))))))

; 1
(define length
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [else (length (cdr ls) (lambda (v) (k (add1 v))))]
      )))

; 2
(define binary-to-decimal
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      ;[else (+ (car n) (* 2 (binary-to-decimal (cdr n))))]
      [else (binary-to-decimal (cdr n) (lambda (v) (k (+ (car n) (* 2 v)))))]
      )))

; 3
(define count-syms*
  (lambda (ls k)
    (cond
     [(null? ls) (k 0)]
    ;[(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
     [(pair? (car ls)) (count-syms* (car ls) (lambda (a) (count-syms* (cdr ls) (lambda (d) (k (+ a d))))))]
    ;[(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
     [(symbol? (car ls)) (count-syms* (cdr ls) (lambda (v) (k (add1 v))))]
     [else (count-syms* (cdr ls) k)]
     )))

; 4
(define tree-sum
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [(pair? (car ls))
       (tree-sum (car ls) (lambda (a)
       (tree-sum (cdr ls) (lambda (d)
         (k (+ a d))))))]
                                         
      ;(+ (tree-sum (car ls)) (tree-sum (cdr ls)))]
     ;[else (+ (car ls) (tree-sum (cdr ls)))]
      [else (tree-sum (cdr ls) (lambda (v) (k (+ (car ls) v))))]
      )))

; 5
(define walk
  (lambda (v ls)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk (cdr p) ls)]
           [else v]))]
      [else v])))

(define walk-cps
  (lambda (v ls k)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk-cps (cdr p) ls k)]
           [else (k v)]))]
      [else (k v)])))

; 6
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
     ;[else (ack (sub1 m) (ack m (sub1 n)))]
      [else (ack-cps m (sub1 n) (lambda (d) (ack-cps (sub1 m) d (lambda (a) (k a)))))]
      )))

; 7
(define fact
  (lambda (n)
    ((lambda (fact) (fact fact n)) (lambda (fact n)
       (cond
         [(zero? n) 1]
         [else (* n (fact fact (sub1 n)))])))))

(define fact-cps
  (lambda (n k)
    ((lambda (fact k) (fact fact n k)) (lambda (fact n k)
       (cond
        [(zero? n) (k 1)]
        [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]
        )) k)))


; 8
#|
(define pascal
  (lambda (n)
    (let ((pascal
           (lambda (pascal)
             (lambda (m a)
               (cond
                 [(> m n) '()]
                 [else (let ((a (+ a m)))
                         (cons a ((pascal pascal) (add1 m) a)))])))))
      ((pascal pascal) 1 0))))
|#
(define pascal-cps
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (lambda (m a k)
               (cond
                 [(> m n) (k '())]
                 [else (let ((a (+ a m)))
                         ((pascal pascal k) (add1 m) a (lambda (c) (k (cons a c)))))])))))
      ((pascal pascal k) 1 0 k))))

; 9

(define empty-s
  (lambda ()
    '()))
 
(define extend-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))
 
(define unify
  (lambda (v w s)
    (let ([v (walk v s)])
      (let ([w (walk w s)])
        (cond
          [(eq? v w) s]
          [(symbol? v) (extend-s v w s)]
          [(symbol? w) (extend-s w v s)]
          [(and (pair? v) (pair? w))
           (let ((s (unify (car v) (car w) s)))
             (cond
               [s (unify (cdr v) (cdr w) s)]
               [else #f]))]
          [(equal? v w) s]
          [else #f])))))



(define empty-s
  (lambda ()
    '()))
 
(define extend-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))
 
(define unify-cps
  (lambda (v w s k)
    (let ([v (walk v s)])
      (let ([w (walk w s)])
        (cond
          [(eq? v w) (k s)]
          [(symbol? v) (k (extend-s v w s))]
          [(symbol? w) (k (extend-s w v s))]
          [(and (pair? v) (pair? w))
           (let ((s (unify-cps (car v) (car w) s k)))
             (cond
               [s (unify-cps (cdr v) (cdr w) s k)]
               [else (k #f)]))]
          [(equal? v w) (k s)]
          [else (k #f)])))))

; 10
#|
(define rember*1
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (rember*1 (car ls)))
          (cons (car ls) (rember*1 (cdr ls)))]
         [else (cons (rember*1 (car ls)) (cdr ls))])]
      [(eq? (car ls) '?) (cdr ls)]
      [else (cons (car ls) (rember*1 (cdr ls)))])))
|#
(define rember*1-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k '())]
     [(pair? (car ls))
      (cond
       [(equal? (car ls) (rember*1-cps (car ls) k)) (rember*1-cps (cdr ls) (lambda (d) 
                                                                      (k (cons (car ls) d))))]
       [else (rember*1-cps (car ls) (lambda (v) (k (cons v (cdr ls)))))])]
     [(eq? (car ls) '?) (k (cdr ls))]
     [else (rember*1-cps (cdr ls) (lambda (v) (k (cons (car ls) v))))])))