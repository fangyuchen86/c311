#|
Samuel Waggoner  srwaggon @ indiana.edu
CSCI C311        Programming Languages
Dan Friedman

Assignment 7
CPS the interpreter
10/11/03 2010.11.03
|#

(load "pmatch.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        Regular          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of
  (lambda (expr env)
    (pmatch expr
      [,n (guard (or (number? n) (boolean? n))) n]
      [,x (guard (symbol? x)) (apply-env env x)]
      [(* ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [(sub1 ,x) (sub1 (value-of x env))]
      [(zero? ,x) (zero? (value-of x env))]
      [(if ,test ,conseq ,alt) (if (value-of test env)
                                   (value-of conseq env)
                                   (value-of alt env))]
      [(letcc ,k-id ,body) (call/cc (lambda (k)
                                      (value-of body (extend-env k-id k env))))]
      [(throw ,v-exp ,k-exp) ((value-of k-exp env) (value-of v-exp env))]
      [(lambda (,id) ,body) (closure id body env)]
      [(,rator ,rand) (apply-proc (value-of rator env) (value-of rand env))])))

(define apply-proc
  (lambda (p a)
    (p a)))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eq? x y)
          a
          (apply-env env y)))))

(define apply-env
  (lambda (env y)
    (env y)))

(define closure
  (lambda (x body env)
    (lambda (a)
      (value-of body (extend-env x a env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Continuation Passing   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-cps
  (lambda (expr env k)
    (pmatch expr
      [,n (guard (or (number? n) (boolean? n))) (k n)]
      [,x (guard (symbol? x)) (apply-env-cps env x k)]
      [(* ,x1 ,x2) (value-of-cps x1 env (lambda (a) (value-of-cps x2 env (lambda (b) (k (* a b))))))]
      [(sub1 ,x) (value-of-cps x env (lambda (a) (k (sub1 a))))]
      [(zero? ,x) (value-of-cps x env (lambda (a) (k (zero? a))))]
      [(if ,test ,conseq ,alt) (value-of-cps test env (lambda (test) (if test
                                                                         (value-of-cps conseq env k)
                                                                         (value-of-cps alt env k))))]
      [(letcc ,k-id ,body) (value-of-cps body (extend-env-cps k-id k env) k)]
      [(throw ,v-exp ,k-exp) (value-of-cps k-exp env (lambda (p) 
                                                       (value-of-cps v-exp env (lambda (a) (p a)))))]
      [(lambda (,id) ,body) (k (closure-cps id body env))]
      [(,rator ,rand) (value-of-cps rator env (lambda (p) 
                                                (value-of-cps rand env (lambda (a) 
                                                                         (apply-proc-cps p a k)))))])))

(define apply-proc-cps
  (lambda (p a k)
    (pmatch p
      [(closure ,x ,body ,env) (value-of-cps body (extend-env-cps x a env) k)])))

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env-cps
  (lambda (x a env)
    `(extend-env ,x ,a ,env)))

(define apply-env-cps
  (lambda (env y k)
    (pmatch env
      [(empty-env) 'error]
      [(extend-env ,x ,a ,env) (if (eq? x y) (k a) (apply-env-cps env y k))]
      ;[(extend-env ,x ,a ,env) (apply-env-cps env y (lambda (d) (k (if (eq? x y) a d))))]
      )))

(define closure-cps
  (lambda (x body env)
    `(closure ,x ,body, env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       Procedural        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-cps-fn
  (lambda (expr env k)
    (pmatch expr
      [,n (guard (or (number? n) (boolean? n))) (apply-k-fn k n)]
      [,x (guard (symbol? x)) (apply-env-cps-fn env x k)]
      [(* ,x1 ,x2) (value-of-cps-fn x1 env (*-outer-k-fn x2 env k))]
      [(sub1 ,x) (value-of-cps-fn x env (sub1-k-fn k))]
      [(zero? ,x) (value-of-cps-fn x env (zero-k-fn k))]
      [(if ,test ,conseq ,alt) (value-of-cps-fn test env (if-k-fn conseq alt env k))]
      [(letcc ,k-id ,body) (value-of-cps-fn body (extend-env-cps-fn k-id k env) k)]
      [(throw ,v-exp ,k-exp) (value-of-cps-fn k-exp env (throw-outer-k-fn v-exp env))]
      [(lambda (,id) ,body) (apply-k-fn k (closure-cps-fn id body env))]
      [(,rator ,rand) (value-of-cps-fn rator env (app-outer-k-fn rand env k))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; all them little helper-critters ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *-inner-k-fn
  (lambda (a k)
    (lambda (b) (apply-k-fn k (* a b)))))

(define *-outer-k-fn
  (lambda (x2 env k)
    (lambda (a) (value-of-cps-fn x2 env (*-inner-k-fn a k)))))

(define sub1-k-fn
  (lambda (k)
    (lambda (a) (apply-k-fn k (sub1 a)))))

(define zero-k-fn
  (lambda (k)
    (lambda (a) (apply-k-fn k (zero? a)))))

(define if-k-fn
  (lambda (conseq alt env k)
    (lambda (test) (if test
                       (value-of-cps-fn conseq env k)
                       (value-of-cps-fn alt env k)))))

(define throw-inner-k-fn
  (lambda (p)
    (lambda (a) (p a))))

(define throw-outer-k-fn
  (lambda (v-exp env)
    (lambda (p) 
      (value-of-cps-fn v-exp env (throw-inner-k-fn p)))))
    
(define app-inner-k-fn
  (lambda (p k)
    (lambda (a) 
      (apply-proc-cps-fn p a k))))

(define app-outer-k-fn
  (lambda (rand env k)
    (lambda (p) 
      (value-of-cps-fn rand env (app-inner-k-fn p k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; all them little data-structural helper.. critters ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define apply-proc-cps-fn
  (lambda (p a k)
    (pmatch p
      [(closure ,x ,body ,env) (value-of-cps-fn body (extend-env-cps-fn x a env) k)])))

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env-cps-fn
  (lambda (x a env)
    `(extend-env ,x ,a ,env)))

(define apply-env-cps-fn
  (lambda (env y k)
    (pmatch env
      [(empty-env) 'error]
      [(extend-env ,x ,a ,env) (if (eq? x y) (apply-k-fn k a) (apply-env-cps-fn env y k))]
      ;[(extend-env ,x ,a ,env) (apply-env-cps-fn env y (lambda (d) (apply-k-fn k (if (eq? x y) a d))))]
      )))

(define closure-cps-fn
  (lambda (x body env)
    `(closure ,x ,body, env)))

(define apply-k-fn
  (lambda (k v)
    (k v)))

(define empty-k 
  (lambda ()
    (let ([okay #t])
      (lambda (v)
        (if okay
            (begin
              (set! okay #f)
              v)
            (errorf 'empty-k "k invoked in non-tail position"))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     Data-Structural     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-cps-ds
  (lambda (expr env k)
    (pmatch expr
      [,n (guard (or (number? n) (boolean? n))) (apply-k-ds k n)]
      [,x (guard (symbol? x)) (apply-env-cps-ds env x k)]
      [(* ,x1 ,x2) (value-of-cps-ds x1 env (*-outer-k-ds x2 env k))]
      [(sub1 ,x) (value-of-cps-ds x env (sub1-k-ds k))]
      [(zero? ,x) (value-of-cps-ds x env (zero-k-ds k))]
      [(if ,test ,conseq ,alt) (value-of-cps-ds test env (if-k-ds conseq alt env k))]
      [(letcc ,k-id ,body) (value-of-cps-ds body (extend-env-cps-ds k-id k env) k)]
      [(throw ,v-exp ,k-exp) (value-of-cps-ds k-exp env (throw-outer-k-ds v-exp env))]
      [(lambda (,id) ,body) (apply-k-ds k (closure-cps-ds id body env))]
      [(,rator ,rand) (value-of-cps-ds rator env (app-outer-k-ds rand env k))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; all them little helper-critters ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-k-ds
  (lambda ()
    `(empty-k)))

(define apply-k-ds
  (lambda (k v)
    (pmatch k
     [(empty-k) v]
     [(*-inner-k-ds ,a ,k) (apply-k-ds k (* a v))]
     [(*-outer-k-ds ,x2 ,env ,k) (value-of-cps-ds x2 env (*-inner-k-ds v k))]
     [(sub1-k-ds ,k) (apply-k-ds k (sub1 v))]
     [(zero-k-ds ,k) (apply-k-ds k (zero? v))]
     [(if-k-ds ,conseq ,alt ,env ,k) (if v
                                         (value-of-cps-ds conseq env k)
                                         (value-of-cps-ds alt env k))]
     [(throw-inner-k-ds ,p) (apply-k-ds p v)]
     [(throw-outer-k-ds ,v-exp ,env) (value-of-cps-ds v-exp env (throw-inner-k-ds v))]
     [(app-inner-k-ds ,p ,k) (apply-proc-cps-ds p v k)]
     [(app-outer-k-ds ,rand ,env ,k) (value-of-cps-ds rand env (app-inner-k-ds v k))]
     )))

(define *-inner-k-ds
  (lambda (a k)
    `(*-inner-k-ds ,a ,k)))

(define *-outer-k-ds
  (lambda (x2 env k)
    `(*-outer-k-ds ,x2 ,env ,k)))

(define sub1-k-ds
  (lambda (k)
    `(sub1-k-ds ,k)))

(define zero-k-ds
  (lambda (k)
    `(zero-k-ds ,k)))

(define if-k-ds
  (lambda (conseq alt env k)
    `(if-k-ds ,conseq ,alt ,env ,k)))

(define throw-inner-k-ds
  (lambda (p)
    `(throw-inner-k-ds ,p)))

(define throw-outer-k-ds
  (lambda (v-exp env)
    `(throw-outer-k-ds ,v-exp ,env)))
    
(define app-inner-k-ds
  (lambda (p k)
    `(app-inner-k-ds ,p ,k)))

(define app-outer-k-ds
  (lambda (rand env k)
    `(app-outer-k-ds ,rand ,env ,k)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; all them little data-structural helper.. critters ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define apply-proc-cps-ds
  (lambda (p a k)
    (pmatch p
      [(closure ,x ,body ,env) (value-of-cps-ds body (extend-env-cps-ds x a env) k)])))

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env-cps-ds
  (lambda (x a env)
    `(extend-env ,x ,a ,env)))

(define apply-env-cps-ds
  (lambda (env y k)
    (pmatch env
      [(empty-env) 'error]
      [(extend-env ,x ,a ,env) (if (eq? x y) (apply-k-ds k a) (apply-env-cps-ds env y k))]
      ;[(extend-env ,x ,a ,env) (apply-env-cps-ds env y (lambda (d) (apply-k-ds k (if (eq? x y) a d))))]
      )))

(define closure-cps-ds
  (lambda (x body env)
    `(closure ,x ,body, env)))
