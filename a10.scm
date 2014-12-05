(define unit_state
  (lambda (a)
    (lambda (s)     ;; <-------------------- this lambda is a Ma               
      `(,a . ,s))))
 
(define star_state
  (lambda (sequel)
    (lambda (ma)
      (lambda (s)   ;; <-------------------- this lambda is a Mb               
        (let ((p (ma s)))
          (let ((new-a (car p)) (new-s (cdr p)))
            (let ((mb (sequel new-a)))
              (mb new-s))))))))

(define rember*evenXhowmanyeven
  (lambda (l)
    (cond
      ((null? l) (unit_state '()))
      ((pair? (car l))
       ((star_state
         (lambda (a)
           ((star_state (lambda (d) (unit_state (cons a d))))
            (rember*evenXhowmanyeven (cdr l)))))
        (rember*evenXhowmanyeven (car l))))
      ((odd? (car l))
       ((star_state (lambda (d) (unit_state (cons (car l) d))))
        (rember*evenXhowmanyeven (cdr l))))
      (else
       ((star_state (lambda (__) (rember*evenXhowmanyeven (cdr l))))
        (lambda (s) `(__ . ,(add1 s))))))))



;1
(define rember*noneXhowmanyeven
  (lambda (l)
    (cond
     [(null? l) (unit_state '())]
     [(pair? (car l))
      ((star_state
        (lambda (a)
          ((star_state (lambda (d) (unit_state (cons a d))))
           (rember*noneXhowmanyeven (cdr l)))))
       (rember*noneXhowmanyeven (car l)))]
     [(odd? (car l))
      ((star_state (lambda (d) (unit_state (cons (car l) d))))
       (rember*noneXhowmanyeven (cdr l)))]
     [else
      ((star_state (lambda (d) (lambda (s) `(,(cons (car l) d) . ,(add1 s)))))
       (rember*noneXhowmanyeven (cdr l)))]
     ;[else
      ;((star_state (lambda (d) (rember*evenXhowmanyeven (cdr l))))
       ;(lambda (s) `(d . ,(add1 s))))]
     )))



;2
(define rember*evenXhowmanyodd
  (lambda (l)
    (cond
      [(null? l) (unit_state '())]
      [(pair? (car l))
       ((star_state
         (lambda (a)
           ((star_state (lambda (d) (unit_state (cons a d))))
            (rember*evenXhowmanyodd (cdr l)))))
        (rember*evenXhowmanyodd (car l)))]
      [(even? (car l))
       ((star_state
         (lambda (d) (unit_state d)))
        (rember*evenXhowmanyodd (cdr l)))]
      [else
       ((star_state (lambda (d) (lambda (s) `(,(cons (car l) d) . ,(add1 s)))))
        (rember*evenXhowmanyodd (cdr l)))]
      )))

; 3.
(define rember*oddXhowmanyeven
  (lambda (l)
    (cond
      [(null? l) (unit_state '())]
      [(pair? (car l))
       ((star_state
         (lambda (a)
           ((star_state (lambda (d) (unit_state (cons a d))))
            (rember*oddXhowmanyeven (cdr l)))))
        (rember*oddXhowmanyeven (car l)))]
      [(odd? (car l))
       ((star_state
         (lambda (d) (unit_state d)))
        (rember*oddXhowmanyeven (cdr l)))]
      [else
       ((star_state (lambda (d) (lambda (s) `(,(cons (car l) d) . ,(add1 s)))))
        (rember*oddXhowmanyeven (cdr l)))]
      )))
(define num3 rember*oddXhowmanyeven)


;4
(define rember*evenXsumofeven
  (lambda (l)
    (cond
      [(null? l) (unit_state '())]
      [(pair? (car l))
       ((star_state
         (lambda (a)
           ((star_state (lambda (d) (unit_state (cons a d))))
            (rember*evenXsumofeven (cdr l)))))
        (rember*evenXsumofeven (car l)))]
      [(even? (car l))
       ((star_state
         (lambda (d) ;(unit_state d)
           (lambda (s)
             `(,d . ,(+ (car l) s)))
                 ))
        (rember*evenXsumofeven (cdr l)))]
      [else
       (
        (star_state 
         (lambda (d) 
           ;(lambda (s) 
             ;`(,(cons (car l) d) . ,s))
           (unit_state (cons (car l) d))
           ))
        (rember*evenXsumofeven (cdr l)))]
      )))
(define num4 rember*evenXsumofeven)

;5
(define rember*evenXdifferenceofoddsandevens
  (lambda (l)
    (cond
      [(null? l) (unit_state '())]
      [(pair? (car l))
       ((star_state
         (lambda (a)
           ((star_state (lambda (d) (unit_state (cons a d))))
            (rember*evenXdifferenceofoddsandevens (cdr l)))))
        (rember*evenXdifferenceofoddsandevens (car l)))]
      [(even? (car l))
       ((star_state
         (lambda (d) ;(unit_state d)
           (lambda (s)
             `(,d . ,(- s (car l))))
                 ))
        (rember*evenXdifferenceofoddsandevens (cdr l)))]
      [else
       (
        (star_state 
         (lambda (d) 
           (lambda (s) 
             `(,(cons (car l) d) . ,(+ s (car l))))
           ;(unit_state (cons (car l) d))
           ))
        (rember*evenXdifferenceofoddsandevens (cdr l)))]
      )))
;6
(define zeroXfact
  (lambda (x)
    (cond
     [(zero? x) (unit_state 0)]
     [else ((star_state
             (lambda (d)
               (lambda (s)
                 `(0 . ,(* s x))))
             ) 
            (zeroXfact (sub1 x))
            )]
     )))

;7
(define binaryXdecimal
  (lambda (l)
    (cond
     [(null? l) (unit_state '())]
     [else;(zero? (car l))
      ((star_state
        (lambda (d)
          (lambda (s)
            `(,(cons (car l) d) . ,(+ (car l) (* 2 s)))
            )))
       (binaryXdecimal (cdr l)))]
     ;[else]
     )))


;8