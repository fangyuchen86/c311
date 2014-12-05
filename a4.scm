#|
Samuel Waggoner
srwaggon@indiana.edu
CSCI-C311
Assignment 4 - Dynamic Scope
2010/10/04

I received assistance in debugging from Gavin Whelan.

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
      [(d-lambda (,x) ,body) (d-closure-fn x body env)]
      [(,rator ,rand) (apply-proc-fn (value-of-fn rator env) (value-of-fn rand env) env)]
      )))

(define apply-proc-fn
  (lambda (proc exp env)
    (proc exp env)
    ))

(define closure-fn
  (lambda (x body env)
    (lambda (a env^)
      (value-of-fn body (extend-env-fn x a env)))))

(define d-closure-fn
  (lambda (x body env)
    (lambda (a env)
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

(define empty-env
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
      [(d-lambda (,x) ,body) (d-closure-ds x body env)]
      [(,rator ,rand) (apply-proc-ds (value-of-ds rator env) (value-of-ds rand env) env)]
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

(define d-closure-ds
  (lambda (x body env)
    `(d-closure-ds ,x ,body ,env)))

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
  (lambda (p a env)
    (pmatch p
     [(closure-ds ,x ,body ,env) (value-of-ds body (extend-env-ds x a env))]
     [(d-closure-ds ,x ,body ,env^) (value-of-ds body (extend-env-ds x a env))]
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

(define let-static
  '(let ([x 2])
     (let ([f (lambda (e) x)])
       (let ([x 5])
         (f 0)))))

(define let-dynamic
  '(let ([x 2])
     (let ([f (d-lambda (e) x)])
       (let ([x 5])
         (f 0)))))

(define value-of value-of-fn)

(test "let-test-static-fn"
      (value-of let-static (empty-env))
      2)

(test "let-test-dynamic-fn"
      (value-of let-dynamic (empty-env))
      5)

(define value-of value-of-ds)

(test "let-test-static-ds"
      (value-of let-static (empty-env))
      2)

(test "let-test-dynamic-ds"
      (value-of let-dynamic (empty-env))
      5)


