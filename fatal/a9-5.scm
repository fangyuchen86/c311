#|
Samuel Waggoner
srwaggon @ indiana.edu
CSCI C311
Programming Languages
Assignment 9-5
Fatal Assignment
Registerize the interpreter

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

(define-union cont
  (empty)
  (if conseq alt env k)
  (mult-inner r1 k)
  (mult-outer rand2 env k)
  (sub1 k)
  (zero k)
  (throw-inner a)
  (throw-outer vexp env)
  (let body env k)
  (apply-inner p k)
  (apply-outer rand env k))

(define expr-)
(define env-)
(define k-)
(define v-)
(define num-)
(define a-)
(define c-)

(define value-of-driver
  (lambda (expr)
    (begin
      (set! k- (cont_empty))
      (set! expr- expr)
      (set! env- (envr_empty))
      (value-of))))


(define value-of
  (lambda () ;expr env k)
    (union-case expr- exp
      [(const n) (begin
                   (set! k- k-)
                   (set! v- n)
                   (apply-k))];(apply-k k n)]
      [(var v) (begin
                 (set! k- k-)
                 (set! env- env-)
                 (set! num- v)
                 (apply-env))];(apply-env env v k)]
      [(if test conseq alt) (begin
                              (set! k- (cont_if conseq alt env- k-))
                              (set! expr- test)
                              (set! env- env-)
                              (value-of))];(value-of test env (cont_if conseq alt env k))]
      [(mult rand1 rand2) (begin
                            (set! k- (cont_mult-outer rand2 env- k-))
                            (set! expr- rand1)
                            (set! env- env-)
                            (value-of))];(value-of rand1 env- (cont_mult-outer rand2 env k-))]
      [(sub1 rand) (begin
                     (set! k- (cont_sub1 k-))
                     (set! expr- rand)
                     (set! env- env-)
                     (value-of))];(value-of rand env- (cont_sub1 k-))]
      [(zero rand) (begin
                     (set! k- (cont_zero k-))
                     (set! expr- rand)
                     (set! env- env-)
                     (value-of))];(value-of rand env- (cont_zero k-))]
      [(letcc body) (begin
                      (set! k- k-)
                      (set! expr- body)
                      (set! env- (envr_extend k- env-))
                      (value-of))];(value-of body (envr_extend k- env-) k-)]
      [(throw vexp kexp) (begin
                           (set! k- (cont_throw-outer vexp env-))
                           (set! expr- kexp)
                           (set! env- env-)
                           (value-of))];(value-of kexp env- (cont_throw-outer vexp env-))]
      [(let vexp body) (begin
                           (set! k- (cont_let body env- k-))
                           (set! expr- vexp)
                           (set! env- env-)
                           (value-of))];(value-of vexp env- (cont_let body env- k-))]
      [(lambda body) (begin
                       (set! k- k-)
                       (set! v- (clos_closure body env-))
                       (apply-k))];(apply-k k- (clos_closure body env-))]
      [(app rator rand) (begin
                          (set! k- (cont_apply-outer rand env- k-))
                          (set! expr- rator)
                          (set! env- env-)
                          (value-of))];(value-of rator env- (cont_apply-outer rand env- k-))]
      )))
      
(define apply-k
  (lambda ();k v
    (union-case k- cont
      [(empty) v-]
      [(if conseq alt env k) (if v-
                                 (begin
                                   (set! k- k)
                                   (set! expr- conseq)
                                   (set! env- env)
                                   (value-of));(value-of conseq env k)
                                 (begin
                                   (set! k- k)
                                   (set! expr- alt)
                                   (set! env- env)
                                   (value-of)))];(value-of alt env k))]
      [(mult-inner r1 k) (begin
                           (set! k- k)
                           (set! v- (* r1 v-))
                           (apply-k))];(apply-k k (* r1 v-))]
      [(mult-outer rand2 env k) (begin
                                  (set! k- (cont_mult-inner v- k))
                                  (set! expr- rand2)
                                  (set! env- env)
                                  (value-of))];(value-of rand2 env (cont_mult-inner v- k))]
      [(sub1 k) (begin
                  (set! k- k)
                  (set! v- (- v- 1))
                  (apply-k))];(apply-k k (- v- 1))]
      [(zero k) (begin
                  (set! k- k)
                  (set! v- (zero? v-))
                  (apply-k))];(apply-k k (zero? v-))]
      [(throw-inner a) (begin
                         (set! k- a)
                         (set! v- v-)
                         (apply-k))];(apply-k a v-)]
      [(throw-outer vexp env) (begin
                                (set! k- (cont_throw-inner v-))
                                (set! expr- vexp)
                                (set! env- env)
                                (value-of))];(value-of vexp env (cont_throw-inner v-))]
      [(let body env k) (begin
                          (set! k- k)
                          (set! expr- body)
                          (set! env- (envr_extend v- env))
                          (value-of))];(value-of body (envr_extend v- env) k)]
      [(apply-inner p k) (begin
                           (set! c- p)
                           (set! a- v-)
                           (set! k- k)
                           (apply-proc))];(apply-proc p v- k)]
      [(apply-outer rand env k) (begin
                                  (set! k- (cont_apply-inner v- k))
                                  (set! expr- rand)
                                  (set! env- env)
                                  (value-of))];(value-of rand env (cont_apply-inner v- k))]
      )))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda () ;env num k
    (union-case env- envr
                [(empty) (error 'env "unbound variable")]
                [(extend arg env)
                 (if (zero? num-)
                     (begin
                       (set! k- k-)
                       (set! v- arg)
                       (apply-k));(apply-k k- arg)
                     (begin 
                       (set! k- k-)
                       (set! env- env)
                       (set! num- (sub1 num-))
                       (apply-env)))];(apply-env env- (sub1 num-) k-))]
                )))

(define-union clos
  (closure code env))

(define apply-proc
  (lambda () ;c a k
    (union-case c- clos
      [(closure code env) (begin
                            (set! k- k-)
                            (set! expr- code)
                            (set! env- (envr_extend a- env))
                            (value-of))];(value-of code (envr_extend a- env) k-)]
      )))
      

                                        ; Factorial of 5...should be 120.

      (pretty-print
       (value-of-driver (exp_app
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
                                        (exp_sub1 (exp_var 0))))))))))
 


                                        ; Test of letcc and throw...should evaluate to 24.
(pretty-print
 (value-of-driver (exp_mult (exp_const 2)
                     (exp_letcc
                      (exp_mult (exp_const 5)
                                (exp_throw (exp_mult (exp_const 2) (exp_const 6))
                                           (exp_var 0)))))))

;; (let ([fact (lambda (f)                                                      
;;               (lambda (n)                                                    
;;                 (if (zero? n)                                                
;;                     1                                                        
;;                     (* n ((f f) (sub1 n))))))])                              
;;   ((fact fact) 5))                                                           

(pretty-print
 (value-of-driver (exp_let
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
            (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5)))))
