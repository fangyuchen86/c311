#|
Samuel Waggoner
srwaggon @ indiana.edu
CSCI - C311
Dan Friedman

Assignment 6 - CPSing.

I received extensive confusing help from Gaven Whelan, Justin Rohl, Claire Alvis, and Małgosia Rada,
and I'm still not confident with the results.  I'd like to go over this more if possible.
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
;; 1
(define length-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k 0)]
     [else (length-cps (cdr ls) (lambda (v) (k (add1 v))))])))

;; 2
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
     [(null? n) (k 0)]
    ; [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))]
     [else (binary-to-decimal-cps (cdr n) (lambda (v) (k (+ (car n) (* 2 v)))))]
     )))

;; 3
(define count-syms*-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k 0)]
    ;[(pair? (car ls)) (+ (count-syms*-cps (car ls)) (count-syms*-cps (cdr ls)))]
     [(pair? (car ls)) (count-syms*-cps (cdr ls) (lambda (d) (count-syms*-cps (car ls) (lambda (a) (k (+ a d))))))]
    ;[(symbol? (car ls)) (add1 (count-syms*-cps (cdr ls)))]
     [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (v) (k (add1 v))))]
    ;[else (count-syms*-cps (cdr ls))]
     [else (count-syms*-cps (cdr ls) (lambda (v) (k v)))]
     )))

;; 4
(define tree-sum-cps
  (lambda (ls k)
    (cond
    ;[(null? ls) 0]
     [(null? ls) (k 0)]
    ;[(pair? (car ls))
    ;  (+ (tree-sum (car ls))
    ;     (tree-sum (cdr ls)))]
     [(pair? (car ls))
      (tree-sum-cps (car ls) (lambda (a)
                           (tree-sum-cps (cdr ls) (lambda (d) (k (+ a d))))))]
    ;[else (+ (car ls) (tree-sum (cdr ls)))]
     [else (tree-sum-cps (cdr ls) (lambda (v) (k (+ (car ls) v))))]
     )))


;; 5
(define walk-cps
  (lambda (v ls k)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk-cps (cdr p) ls k)]
           [else (k v)]))]
      [else (k v)])))

;; 6
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
     ;[else (ack-cps (sub1 m) (ack-cps m (sub1 n)))])))
      [else (ack-cps m (sub1 n) (lambda (v) (ack-cps (sub1 m) v k)))]
      )))

;; 7
(define fact-cps
  (lambda (n k)
    ((lambda (fact k) (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact fact (sub1 n) (lambda (v) (k (* n v))))])) k)))

;; 8
(define pascal-cps
  (lambda (n k)
    (let ([pascal
           (lambda (pascal k)
             (k (lambda (m a k)
               (cond
                 [(> m n) (k '())]
                 [else (let ([a (+ a m)])
                         (pascal pascal (lambda (v) (v (add1 m) a (lambda (d) (k (cons a d)))))))]))))])
      (pascal pascal (lambda (v) (v 1 0 k))))))

;; 9
; didn't even start...
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

;; 10
; this doesn't work.  blah.
(define rember*1-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k '())]
     [(pair? (car ls)) (rember*1-cps (car ls) (lambda (v)
      (cond 
       [(equal? (car ls) v)
       ;(cons (car ls) (rember*1-cps (cdr ls)))]
        (rember*1-cps (cdr ls) (lambda (d) (k (cons (car ls) d))))]
       [else (k (cons v (cdr ls)))])))]
     [(eq? (car ls) '?) (k (cdr ls))]
     [else (rember*1-cps (cdr ls) (lambda (v) (k (cons (car ls) v))))])))

; see lambda? add k
; simple procedure, wrap in k
; make complex procedure into tail-call, pass (lambda (v) .. as third arg
; 