#|
Samuel Waggoner
srwaggon @ indiana.edu
CSCI C311
Programming Languages

Assignment 9-2
Fatal Assignment
Representation-independent with respect to continuations

10/11/7
|#

(load "parenthec.ss")
(define-union exp
        (const n)
        (var v)
        (if test conseq alt)
        (mult rand1 rand2)
        (sub1 rand)
        (zero rand)
        (letcc body)
        (throw vexp kexp)
        (let vexp body)
        (lambda body)
        (app rator rand))

(define value-of
  (lambda (expr env k)
    (union-case expr exp
                [(const n) (apply-k k n)]
                [(var v) (apply-env env v k)]
                [(if test conseq alt)
                 (value-of test env (if-k conseq alt env k))]
                [(mult rand1 rand2) (value-of rand1 env (mult-outer-k rand2 env k))]
                [(sub1 rand) (value-of rand env (sub1-k k))]
                [(zero rand) (value-of rand env (zero-k k))]
                [(letcc body) (value-of body (envr_extend k env) k)]
                [(throw vexp kexp)
                 ;((value-of kexp env) (value-of vexp env))]
                  (value-of kexp env (throw-outer-k vexp env))]
                [(let vexp body)
                 ;(value-of body (envr_extend (value-of vexp env) env))]
                 (value-of vexp env (let-k body env k))]
                [(lambda body) (apply-k k (clos_closure body env))]
                [(app rator rand)
                 ;(apply-proc (value-of rator env) (value-of rand env))
                 (value-of rator env (apply-outer-k rand env k))
                 ])))

(define-union cont
  (empty v))

(define-union envr
  (empty)
  (extend arg env))


(define apply-k
  (lambda (k v)
    (k v)))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define if-k
  (lambda (conseq alt env k)
    (lambda (a) (if a
                    (value-of conseq env k)
                    (value-of alt env k)))))

(define mult-inner-k
  (lambda (r1 k)
    (lambda (r2)
      (apply-k k (* r1 r2)))))

(define mult-outer-k
  (lambda (rand2 env k)
    (lambda (r1)
      (value-of rand2 env (mult-inner-k r1 k)))))

(define sub1-k
  (lambda (k)
    (lambda (a) (apply-k k (- a 1)))))

(define zero-k
  (lambda (k)
    (lambda (a) (apply-k k (zero? a)))))

(define throw-inner-k
  (lambda (a)
    (lambda (b) (a b))))

(define throw-outer-k
  (lambda (vexp env)
    (lambda (a) (value-of vexp env (throw-inner-k a)))))

(define let-k
  (lambda (body env k)
    (lambda (a) (value-of body (envr_extend a env) k))))

(define apply-inner-k
  (lambda (p k)
    (lambda (a) (apply-proc p a k))))

(define apply-outer-k
  (lambda (rand env k)
    (lambda (p) (value-of rand env (apply-inner-k p k)))))



(define apply-env
  (lambda (env num k)
    (union-case env envr
                [(empty) (error 'env "unbound variable")]
                [(extend arg env)
                 (if (zero? num)
                     (apply-k k arg)
                     (apply-env env (sub1 num) k))])))

(define-union clos
  (closure code env))

(define apply-proc
        (lambda (c a k)
          (union-case c clos
                      [(closure code env) (value-of code (envr_extend a env) k)])))


                                        ; Factorial of 5...should be 120.
      (pretty-print
       (value-of (exp_app
                  (exp_lambda
                   (exp_app
                    (exp_app (exp_var 0) (exp_var 0))
                    (exp_const 5)))
                  (exp_lambda
                   (exp_lambda
                    (exp_if (exp_zero (exp_var 0))
                            (exp_const 1)
                            (exp_mult (exp_var 0)
                                      (exp_app
                                       (exp_app (exp_var 1) (exp_var 1))
                                       (exp_sub1 (exp_var 0))))))))
                 (envr_empty) (empty-k)))

                                        ; Test of letcc and throw...should evaluate to 24.
(pretty-print
 (value-of (exp_mult (exp_const 2)
                     (exp_letcc
                      (exp_mult (exp_const 5)
                                (exp_throw (exp_mult (exp_const 2) (exp_const 6))
                                           (exp_var 0)))))
           (envr_empty) (empty-k))) 

;; (let ([fact (lambda (f)                                                      
;;               (lambda (n)                                                    
;;                 (if (zero? n)                                                
;;                     1                                                        
;;                     (* n ((f f) (sub1 n))))))])                              
;;   ((fact fact) 5))                                                           

(pretty-print
 (value-of (exp_let
            (exp_lambda
             (exp_lambda
              (exp_if
               (exp_zero (exp_var 0))
               (exp_const 1)
               (exp_mult
                      (exp_var 0)
                      (exp_app
                       (exp_app (exp_var 1) (exp_var 1))
                       (exp_sub1 (exp_var 0)))))))
            (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5)))
                 (envr_empty) (empty-k)))
