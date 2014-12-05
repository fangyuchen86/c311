#|
Samuel Waggoner
CSCI C311
Assignment 8
Registerisation and Trampolining.
|#

(load "pmatch.scm")

(define empty-k-fn
  (lambda ()
    (lambda (v) v)))

(define empty-k
  (lambda ()
    `(empty-k)))

(define trampoline
  (lambda (proc)
    (trampoline (proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         depth-cps          ; Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define depth
  (lambda (ls k)
    (cond
     [(null? ls) (k 1)]
     [(pair? (car ls))
      (depth (car ls)
             (lambda (l)
               (depth (cdr ls)
                      (lambda (r)
                        (let ((l (add1 l)))
                          (k (if (< l r) r l)))))))]
     [else (depth (cdr ls) k)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         depth-ds           ; Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-k-ds
  (lambda (k v)
    (pmatch k
      [(empty-k) v]
      [(depth-outer-k ,ls ,k) (depth-ds (cdr ls) (depth-inner-k-ds v k))]
      [(depth-inner-k ,l ,k) (let ((l (add1 l)))
                             (apply-k-ds k (if (< l v) v l)))])))

(define depth-ds
  (lambda (ls k)
    (cond
     [(null? ls) (apply-k-ds k 1)]
     [(pair? (car ls))
      (depth-ds (car ls) (depth-outer-k-ds ls k))]
     [else (depth-ds (cdr ls) k)])))

(define depth-outer-k-ds
  (lambda (ls k)
    `(depth-outer-k ,ls ,k)))

(define depth-inner-k-ds
  (lambda (l k)
    `(depth-inner-k ,l ,k)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         depth-reg          ; Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define k*)
(define v*)
(define ls*)

(define apply-k-reg
  (lambda () ;k v
    (pmatch k*
      [(empty-k) v*]
      [(depth-outer-k ,ls ,k) (begin
                                (set! ls* (cdr ls))
                                (set! k* (depth-inner-k v* k))
                                (depth-reg));(depth-reg (cdr ls) (depth-inner-k v k))
       ]
      [(depth-inner-k ,l ,k) (let ((l (add1 l)))
                               (begin
                                 (set! k* k)
                                 (set! v* (if (< l v*) v* l))
                                 (apply-k-reg));(apply-k-reg k (if (< l v) v l))
                               )])))

(define depth-reg
  (lambda () ;ls k
    (cond
     [(null? ls*) (begin
                   (set! k* k*)
                   (set! v* 1)
                   (apply-k-reg))
                  ;(apply-k-reg k 1)]
                  ]
     [(pair? (car ls*))
      (begin
        (set! k* (depth-outer-k ls* k*))
        (set! ls* (car ls*))
        (depth-reg));(depth-reg (car ls) (depth-outer-k ls k))]
      ]
     [else (begin
             (set! ls* (cdr ls*))
             (set! k* k*)
             (depth-reg));(depth-reg (cdr ls) k)
                 ])))



(define depth-outer-k
  (lambda (ls k)
    `(depth-outer-k ,ls ,k)))

(define depth-inner-k
  (lambda (l k)
    `(depth-inner-k ,l ,k)))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! k* (empty-k))
      (set! ls* ls)
      (depth-reg))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        depth-tramp         ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define apply-k-ds
  (lambda (k v)
    (pmatch k
      [(empty-k) v]
      [(depth-outer-k ,ls ,k) (depth-ds (cdr ls) (depth-inner-k-ds v k))]
      [(depth-inner-k ,l ,k) (let ((l (add1 l)))
                             (apply-k-ds k (if (< l v) v l)))])))

(define depth-ds
  (lambda (ls k)
    (cond
     [(null? ls) (apply-k-ds k 1)]
     [(pair? (car ls))
      (depth-ds (car ls) (depth-outer-k-ds ls k))]
     [else (depth-ds (cdr ls) k)])))

(define depth-outer-k-ds
  (lambda (ls k)
    `(depth-outer-k ,ls ,k)))

(define depth-inner-k-ds
  (lambda (l k)
    `(depth-inner-k ,l ,k)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         ack-cps            ;  Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ack
  (lambda (n m k)
    (cond
     [(zero? n) (k (add1 m))]
     [(zero? m) (ack (sub1 n) 1 k)]
     [else (ack n (sub1 m) (lambda (v)
                             (ack (sub1 n) v k)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         ack-ds             ; Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ack-ds
  (lambda (n m k)
    (cond
     [(zero? n) (ack-apply-k k (add1 m))]
     [(zero? m) (ack-ds (sub1 n) 1 k)]
     [else (ack-ds n (sub1 m) (ack-inner-k n k))]
     )))

(define ack-inner-k
  (lambda (n k)
    `(ack-inner-k ,n ,k)))

(define ack-apply-k
  (lambda (k v)
    (pmatch k
      [(empty-k) v]
      [(ack-inner-k ,n ,k) (ack-ds (sub1 n) v k)]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         ack-reg            ; Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define n*)
(define m*)
(define k*)
(define v*)

(define ack-reg
  (lambda () ;n m k
    (cond
     [(zero? n*) (begin
                  (set! k* k*)
                  (set! v* (add1 m*))
                  (ack-apply-k-reg))];(ack-apply-k k (add1 m))]
     [(zero? m*) (begin
                  (set! k* k*)
                  (set! n* (sub1 n*))
                  (set! m* 1)
                  (ack-reg))];(ack-reg (sub1 n) 1 k)]
     [else (begin
             (set! k* (ack-inner-k n* k*))
             (set! n* n*)
             (set! m* (sub1 m*))
             (ack-reg))];(ack-reg n (sub1 m) (ack-inner-k n k))]
     )))

(define ack-inner-k
  (lambda (n k)
    `(ack-inner-k ,n ,k)))

(define ack-apply-k-reg
  (lambda () ;k v
    (pmatch k*
      [(empty-k) v*]
      [(ack-inner-k ,n ,k) (begin
                             (set! k* k)
                             (set! n* (sub1 n))
                             (set! m* v*)
                             (ack-reg))];(ack-reg (sub1 n) v k)]
      )))

(define ack-reg-driver
  (lambda (n m)
    (begin
      (set! k* (empty-k))
      (set! n* n)
      (set! m* m)
      (ack-reg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         fact-cps           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
        [(zero? n) (k 1)]
        [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
     k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         fact-ds            ;  Works...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fact-ds
  (lambda (n k)
    ((lambda (fact k) (fact fact n k))
     (lambda (fact n k)
       (cond
        [(zero? n) (fact-apply-k k 1)]
        [else (fact fact (sub1 n) (fact-k n k))]))
     k)))

(define fact-k
  (lambda (n k)
    `(fact-k ,n ,k)))

(define fact-apply-k
  (lambda (k v)
    (pmatch k
     [(empty-k) v]
     [(fact-k ,n ,k) (fact-apply-k k (* n v))]
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         fact-reg           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define n-)
(define k-)
(define v-)

(define fact-reg
  (lambda () ;n k
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
        [(zero? n) (fact-apply-k k 1)]
        [else (fact fact (sub1 n) (fact-k n k))]))
     k)))

(define fact-k
  (lambda (n k)
    `(fact-k ,n ,k)))

(define fact-apply-k-reg
  (lambda () ;k v
    (pmatch k-
     [(empty-k) v-]
     [(fact-k ,n ,k) (begin
                       (set! k- k)
                       (set! v- (* n v-))
                       (fact-apply-k-reg))];(fact-apply-k-reg k (* n v))]
     )))

(define fact-reg-driver
  (lambda (n)
    (begin
      (set! n- n)
      (set! k- (empty-k))
      (fact-reg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        power-cps           ; Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define power
  (lambda (x n k)
    (cond
     [(= n 0) (k 1)]
     [(odd? n) (power x (sub1 n) (lambda (v)
                                   (k (* x v))))]
     [(even? n) (power x (/ n 2) (lambda (v)
                                   (k (* v v))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        power-cps           ; working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define power-ds
  (lambda (x n k)
    (cond
     [(= n 0) (power-apply-k k 1)]
     [(odd? n) (power-ds x (sub1 n) (power-odd-k x k))]
     [(even? n) (power-ds x (/ n 2) (power-even-k k))])))

(define power-odd-k
  (lambda (x k)
    `(power-odd-k ,x ,k)))

(define power-even-k
  (lambda (k)
    `(power-even-k ,k)))

(define power-apply-k
  (lambda (k v)
    (pmatch k
     [(empty-k) v]
     [(power-odd-k ,x ,k) (power-apply-k k (* x v))]
     [(power-even-k ,k) (power-apply-k k (* v v))]
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        power-reg           ; Working.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x-)
(define n-)
(define k-)
(define v-)


(define power-reg
  (lambda () ;x n k
    (cond
     [(= n- 0) (begin
                 (set! k- k-)
                 (set! v- 1)
                 (power-apply-k-reg))] ;(power-apply-k-reg k 1)]
     [(odd? n-) (begin
                 (set! k- (power-odd-k x- k-))
                 (set! x- x-)
                 (set! n- (sub1 n-))
                 (power-reg))] ;(power-reg x (sub1 n) (power-odd-k x k))]
     [(even? n-) (begin
                   (set! k- (power-even-k k-))
                   (set! x- x-)
                   (set! n- (/ n- 2))
                   (power-reg))];(power-reg x (/ n 2) (power-even-k k))]
     )))

(define power-odd-k
  (lambda (x k)
    `(power-odd-k ,x ,k)))

(define power-even-k
  (lambda (k)
    `(power-even-k ,k)))

(define power-apply-k-reg
  (lambda () ;k v
    (pmatch k-
     [(empty-k) v-]
     [(power-odd-k ,x ,k) (begin
                            (set! k- k)
                            (set! v- (* x- v-))
                            (power-apply-k-reg))];(power-apply-k-reg k (* x- v-))]
     [(power-even-k ,k) (begin
                          (set! k- k)
                          (set! v- (* v- v-))
                          (power-apply-k-reg))];(power-apply-k-reg k (* v- v-))]
     )))

(define power-reg-driver
  (lambda (x n)
    (begin
      (set! x- x)
      (set! n- n)
      (set! k- (empty-k))
      (power-reg))))