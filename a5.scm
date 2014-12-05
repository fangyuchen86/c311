#|
Samuel Waggoner
srwaggon
CSCI C311
Dan Friedman
Assignment 5
2010 / 10 / 20

I received some help from Gavin Whelan, Malgosia Rada, Dustin
Dannensomething, and Justin Rohl.
|#

(load "pmatch.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          Call by Value         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define val-of-cbv
  (lambda (exp env)
    (pmatch exp
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [,x (guard (symbol? x)) (unbox (apply-env env x))]
      [(zero? ,n) (zero? (val-of-cbv n env))]
      [(sub1 ,n) (sub1 (val-of-cbv n env))]
      [(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [(lambda (,x) ,body) (closure-cbv x body env)]
      [(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                   (val-of-cbv conseq env)
                                   (val-of-cbv alt env))]
      [(set! ,x ,a) (let ([b (apply-env env x)])
                      (set-box! b (val-of-cbv a env)))]
      [(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [(random ,n) (random (val-of-cbv n env))]
      [(,rator ,rand) (apply-proc (val-of-cbv rator env)
                                  (box (val-of-cbv rand env)))]
      )))
    
(define closure-cbv
  (lambda (x body env)
    (lambda (a)
      (val-of-cbv body (extend-env x a env)))))

(define empty-env
  (lambda ()
    (lambda (y)
      (errorf 'empty-env "free variable: ~s" y))))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eq? x y)
          a
          (apply-env env y)))))

(define apply-env
  (lambda (env x)
    (env x)))

(define apply-proc
  (lambda (rator rand)
    (rator rand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      Call by Reference         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define val-of-cbr
  (lambda (exp env)
    (pmatch exp
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [,x (guard (symbol? x)) (unbox (apply-env env x))]
      [(zero? ,n) (zero? (val-of-cbr n env))]
      [(sub1 ,n) (sub1 (val-of-cbr n env))]
      [(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [(lambda (,x) ,body) (closure-cbr x body env)]
      [(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                   (val-of-cbr conseq env)
                                   (val-of-cbr alt env))]
      [(set! ,x ,a) (let ([b (apply-env env x)])
                      (set-box! b (val-of-cbr a env)))]
      [(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [(random ,n) (random (val-of-cbr n env))]
      [(,rator ,x) (guard (symbol? x))
       (apply-proc (val-of-cbr rator env) (apply-env env x))]
      [(,rator ,rand) (apply-proc (val-of-cbr rator env)
                                  (box (val-of-cbr rand env)))]
      )))
    
(define closure-cbr
  (lambda (x body env)
    (lambda (a)
      (val-of-cbr body (extend-env x a env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          Call by Name         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    


(define val-of-cbname
  (lambda (exp env)
    (pmatch exp
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [,x (guard (symbol? x)) ((unbox (apply-env env x)))]
      [(zero? ,n) (zero? (val-of-cbname n env))]
      [(sub1 ,n) (sub1 (val-of-cbname n env))]
      [(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [(lambda (,x) ,body) (closure-cbname x body env)]
      [(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                   (val-of-cbname conseq env)
                                   (val-of-cbname alt env))]
      [(random ,n) (random (val-of-cbname n env))]
      [(,rator ,rand) (apply-proc (val-of-cbname rator env)
                                  (box (lambda () (val-of-cbname rand env))))]
      )))
    
(define closure-cbname
  (lambda (x body env)
    (lambda (a)
      (val-of-cbname body (extend-env x a env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          Call by Need          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define val-of-cbneed
  (lambda (exp env)
    (pmatch exp
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [,x (guard (symbol? x)) (let ([b (apply-env env x)])
                                (let ([v ((unbox b))])
                                  (begin
                                    (set-box! b (lambda () v))
                                    v)))]
      [(zero? ,n) (zero? (val-of-cbneed n env))]
      [(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [(lambda (,x) ,body) (closure-cbneed x body env)]
      [(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                   (val-of-cbneed conseq env)
                                   (val-of-cbneed alt env))]
      [(random ,n) (random (val-of-cbneed n env))]
      [(,rator ,rand) (apply-proc (val-of-cbneed rator env)
                                  (box (lambda () (val-of-cbneed rand env))))]
      )))
    
(define closure-cbneed
  (lambda (x body env)
    (lambda (a)
      (val-of-cbneed body (extend-env x a env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       TESTS TESTS TESTS        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;; Making sure set! works
(test "set!"
  (val-of-cbr
   '((lambda (x) (begin2 (set! x #t)
                         (if x 3 5))) #f)
   (empty-env))
  3)
 
;; Returns 4 under CBR...
(test "interesting-cbr-1"
  (val-of-cbr
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
  4)
 
;; ...but returns 3 under CBV.
(test "interesting-cbv-1"
  (val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
  3)
 
;; returns 44 under CBR...
(test "interesting-cbr-2"
  (val-of-cbr
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
  44)
 
;; ...but returns 55 under CBV!  You can change the "begin2" to
;; "begin" and evaluate this in the Scheme REPL as evidence that
;; Scheme uses CBV.
(test "interesting-cbv-2"
  (val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
  55)
 
;; Returns 44 under CBR...
(test "interesting-cbr-3"
  (val-of-cbr
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))
  44)
 
;; ...but returns 33 under CBV.
(test "interesting-cbv-3"
  (val-of-cbv
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))
  33)
 
(define random-sieve
  '((lambda (n)
      (if (zero? n)
          (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
          (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
    (random 2)))
 
;; call-by-name
;;P(false positive) ≤ .01                                                     
(test "random-by-name"
  (val-of-cbname random-sieve (empty-env)) #f)
 
;; call-by-need                                                                  
(test "random-by-need"
  (val-of-cbneed random-sieve (empty-env)) #t)
 
;; Does not terminate with val-of-cbr or val-of-cbv -- try it!
(test "omega-by-name"
  (val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env))
  100)

