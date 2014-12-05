#|
Samuel Waggoner
srwaggon@indiana.edu
CSCI-C311
Assignment 3
2010/10/04

... Countless hours of help from countless people contributed to me turning this in.
Thanks again, Professor Friedman, for spending three hours on it with me today.
|#

(load "pmatch.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     Functional Representation     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-fn
  (lambda (exp env)
    (pmatch exp
      [,x (guard (symbol? x)) (apply-env-fn env x)]
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [(zero? ,x) (zero? (value-of-fn x env))]
      [(sub1 ,x) (sub1 (value-of-fn x env))]
      [(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env))]
      [(let ([,x ,y]) ,body) (value-of-fn body (extend-env-fn x (value-of-fn y env) env))]
      [(if ,test ,conseq ,alt) (if (value-of-fn test env)
                                   (value-of-fn conseq env)
                                   (value-of-fn alt))]
      [(+ ,n-exp1 ,n-exp2) (+ (value-of-fn n-exp1 env)(value-of-fn n-exp2 env))]
      [(lambda (,x) ,body) (closure-fn x body env)]
      [(,rator ,rand) (apply-proc-fn (value-of-fn rator env) (value-of-fn rand env))]
      )))

(define apply-proc-fn
  (lambda (proc exp)
    (proc exp)
    ))

(define closure-fn
  (lambda (x body env)
    (lambda (a)
      (value-of-fn body (extend-env-fn x a env)))))

(define apply-env-fn
  (lambda (env y)
    (env y)
    ))

(define extend-env-fn
  (lambda (x a env)
    (lambda (y)
      (if (eq? x y)
          a
          (apply-env-fn env y)
          ))))

(define empty-env-ds
  (lambda ()
    (lambda (y)
      (errorf "error!" "Unbound Variable")
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     Data-structural Representation     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-ds
  (lambda (exp env)
    (pmatch exp
      [,x (guard (symbol? x)) (apply-env-ds env x)]
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [(zero? ,x) (zero? (value-of-ds x env))]
      [(sub1 ,x) (sub1 (value-of-ds x env))]
      [(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [(let ([,x ,y]) ,body) (value-of-ds body (extend-env-ds x (value-of-ds y env) env))]
      [(if ,test ,conseq ,alt) (if (value-of-ds test env)
                                   (value-of-ds conseq env)
                                   (value-of-ds alt))]
      [(+ ,n-exp1 ,n-exp2) (+ (value-of-ds n-exp1 env)(value-of-ds n-exp2 env))]
      [(lambda (,x) ,body) (closure-ds x body env)]
      [(,rator ,rand) (apply-proc-ds (value-of-ds rator env) (value-of-ds rand env))]
      )))

(define empty-env-ds
  (lambda ()
    'error))

(define extend-env-ds
  (lambda (x a env)
    `(extend-env-ds ,x ,a ,env)
    ))

(define closure-ds
  (lambda (x body env)
    `(closure-ds ,x ,body ,env)))

(define apply-env-ds
  (lambda (env y)
    (pmatch env
     [(empty-env-ds) 'error]
     [(extend-env-ds ,x ,a ,env)
      (if (eq? x y)
          a
          (apply-env-ds env y))]
     )))

(define apply-proc-ds
  (lambda (p a)
    (pmatch p
     [(closure-ds ,x ,body ,env) (value-of-ds body (extend-env-ds x a env))]
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;              Test Cases!               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
           (printf "~s works!\n" title)
           (errorf
            'test
            "Failed ~s: ~a\nExpected: ~a\nComputed: ~a"
            title 'tested-expression expected produced))))))
 
(define run-tests
  (lambda ()
    (test "let-a"
      (value-of
       '(let ([y (* 3 4)])
          ((lambda (x) (* x y)) (sub1 6)))
       (empty-env))
      60)
    (test "let-b"
      (value-of
       '(let ([x (* 2 3)])
          (let ([y (sub1 x)])
            (* x y)))
       (empty-env))
      30)
    (test "let-c"
      (value-of
       '(let ([x (* 2 3)])
          (let ([x (sub1 x)])
            (* x x)))
       (empty-env))
      25)))
 
(define value-of value-of-fn)
 
(run-tests)
 
(define value-of value-of-ds)
 
(run-tests)