#|
Samuel Waggoner
srwaggon@indiana.edu
CSCI-C311

I got extensive help from Małgosia Rada

|#



(load "pmatch.scm")

#|

(define free?
  (lambda (y E)
    (pmatch E
            [,x (guard (symbol? x)) (eq? x y)]
            [(lambda (,x) ,E1) (and (not (eq? x y)) (free? y E1))]
            [(,E1 ,E2) (or (free? y E1) (free? y E2))])))

(define bound?
  (lambda (y E)
    (pmatch E
            [,x (guard (symbol? x)) #f]
            [(lambda (,x) ,E1) (or (and (eq? x y) (free? y E1)) (bound? y E1))]
            [(,E1 ,E2) (or (bound? y E1) (bound? y E2))])))

|#

(define walk-symbol
  (lambda (x exp)
    (let ([a (assq x exp)])
      (pmatch a
        [(,y . ,s) (walk-symbol s exp)]
        [,z x]))))

(define free
  (lambda (exp)
    (letrec
      ([oc (lambda (e ans vars)
             (pmatch e
                     [,x (guard (symbol? x)) 
                       (if (member x vars)
                           ans
                           (cons x ans))]
                     [(lambda (,x) ,body) 
                       (oc body ans (cons x vars))]
                     [(,rator ,rand) 
                       (union (oc rator ans vars) (oc rand ans vars))]))])
      (oc exp '() '()))))

(define bound
  (lambda (exp)
    (letrec
      ([oc (lambda (e ans vars)
             (pmatch e
                     [,x (guard (symbol? x)) 
                       (if (member x vars)
                           (cons x ans)
                           ans)]
                     [(lambda (,x) ,body) 
                       (oc body ans (cons x vars))]
                     [(,rator ,rand) 
                       (union (oc rator ans vars) (oc rand ans vars))]))])
      (oc exp '() '()))))

(define union
  (lambda (x y)
    (cond
     [(null? x) y]
     [(member (car x) y) (union (cdr x) y)]
     [else (cons (car x) (union (cdr x) y))])))

(define lex
  (lambda (exp)
    (let ([bound-ls (bound exp)])
      (letrec
          ([helper (lambda (e ans)
            (pmatch e
              [,x (guard (symbol? x)) 
                (if (member x bound-ls)
                    (list 'var (get-index x ans))
                    (list 'free-var x))]
              [(lambda (,x) ,body) 
               (list 'lambda (helper body (cons x ans)))]
              [(,rator ,rand)
               (list (helper rator ans)
                     (helper rand ans))]))])
        (helper exp '())))))

 (define get-index
   (lambda (sym ls)
     (cond
      [(eq? sym (car ls)) 0]
      [else (add1 (get-index sym (cdr ls)))])))
  
(define both?
 (lambda (x exp)
   (if (and (member x (free exp)) (member x (bound exp)))
       #t
       #f)))