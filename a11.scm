#|
Samuel Waggoner
srwggon @ indiana.edu

CSCI-C311
Programming Languages

Assignment 11
Logical Programming
|#

(load "mk.scm")
(load "mkdefs.scm")

#|   (1)
This program starts by checking the association between 5 and q.
Since q is not yet associated, it is associated with the value 5.
Next, it hits the first conde statement, which throws conde2.
The second conde is the goal of the first conde,
which checks the association between 5 and q (success!),
whose success then tries to check the association between 6 and q (fail!).
That means that the second conde throws a failure to the first one.
The consequence of the first conde, (== 5 q), then is not evaluated.
The first conde then tries its second.. thing..,
which again checks the evaluation between q and 5 (success!),
and thus is our only answer (5).
This 5 is then printed in the list of answers and appears on console as (5).
|#
(define one
  (lambda ()
    (run 2 (q)
         (== 5 q)
         (conde
          [(conde
            [(== 5 q) (== 6 q)]) (== 5 q)]
        [(== q 5)]))
    ))


#|  (2)
The program begins with a run*, meaning it will return all answers available.
It then births two fresh variables, x and y.
The value of q is associated with the constructed list with x's evaluation as the car,
and with the evaluation of y as the cadr.
Since this association is made, we have found our first answer.
q = (_.0 _.1), because both x and y are fresh and hold no current values, but are separate variables.
We enter the first conde whose first line's goal is a fail.  It does not return an answer.
The program pretends ignores the first goal of the conde and proceeds to the rest.
We enter the second conde line, whose first line associates x with 5, and y with x, which is 5.
This makes our second answer: (5 5).
The program continues to evaluate the conde lines in search of answers.
It checks the association between the list containing the evaluation of x (currently fresh: _.0),
and y (also fresh).
Since y is fresh, the evaluation of the list containing the evaluation of x is assigned to y,
and is returned as an answer.
The final goal of the second conde is to check the association between x and y,
pretending that all previous goals of this conde have failed.
Since both x and y are fresh, they are associated together.
This association (_.0 ._0) is returned as an answer.

The final goal of the first conde is succeed, which obviously succeeds.
However, it says nothing of either x or y, therefore
there is no new answer.
|#
(define two
  (lambda ()
    (run* (q)
          (exist (x y)
                 (== `(,x ,y) q)
                 (conde
                  [fail succeed]
                  [(conde
                    [(== 5 x) (== y x)]
                    [(== `(,x) y)]
                    [(== x y)])]
                  [succeed])))))

#|
This program is expecting at most one answer.
The program initiates by checking the alwayso for a successful goal.
alwayso is essentially a trampolined success.
The program finds several answers but never finishes finding them.
Therefore, the fail line never evaluates.
If the fail line could evaluate, then the entire program would be deemed failed.
And we would have no answers.
However, since the program is locked trampolining over successes,
We find infinite answers.  None of which alter q.
And the program goes into an infinite loop of success.
What a terrible way to crash and burn.
|#
(define three
  (lambda ()
    (run 1 (q)
         alwayso
         fail)))

#|
This program is expecting at most one answer.
It hits the fail line and finds that it fails.
This means that the entire 'run' has failed.
We do not evaluate the following lines because we have already failed.
We have no answers.
We have no cake.
|#
(define four
  (lambda ()
    (run 1 (q)
         fail
         alwayso)))


#|
The program expects at most two answers.
It hits the conde line which has two cases.
The succeed line of the conde succeeds but does not alter q.
Therefore out first answer is _.0.
The program then pretends that the answer was not found and evaluates
the rest of the conde for answers.
It runs into the nevero, which essentially trampolines a big fat fail.
Similar to those you've seen on youtube.
Again, we are caught in an infinite loop, this time of fail.
|#
(define five
  (lambda ()
    (run 2 (q)
         (conde
          [succeed]
          [nevero]))
    ))

#|
This program will accept all valid answers.
It creates four fresh variables from an exist.
It sets the value of 'a' to the evaluation of 'c',
and sets the value of 'b' to the pair containing the evaluation of 'a' as the car
and 'd' as the cdr.
We still no nothing about the values of any of the variables other than their associations.
On the next line, we associate b with the list containing a.
We also associate c with d.
For b to be associated with both the pair containing 'a' as the car and d with the cdr,
as well as for 'b' to be associated with the list containing 'a', it requires that 'd'
is associated with the empty list.
Since 'c' is also associated with 'd', 'c' is also the empty list.
Since 'a' is also associated with 'c', 'a' is also the empty list.
This means that 
'a' is associated with the empty list,
'b' is associated with the list containing the empty list,
'c' is associated with the empty list,
'd' is associated with the empty list.

Since the answer is the list containing 'a', 'b', 'c', and 'd',
the answer (the value associated with q) is therefore (() (()) () ()).
|#
(define six
  (lambda ()
    (run* (q)
          (exist (a b c d)
                 (== `(,a ,b) `(,c (,a . ,d)))
                 (== `(,b ,c) `((,a) ,d))
                 (== `(,a ,b ,c ,d) q)))
    ))

(define membero
  (lambda (x ls)
    (cond
     [(nullo ls) fail]
     [(exist (a)
             (== (caro ls) a)
             (== a x) success)]
     [(exist (d)
             (== (cdro ls) d)
             (membero x d))])))

(define one-itemo
  (lambda (x s out)
    (conde
      [(nullo s) (== '() out)]
      [(exist (a d mid res) 
              (conso a d s)
              (conso x a mid)
              (conso mid res out)
              (one-itemo x d res))
       ]
      )))

(define one-item
  (lambda (x s)
    (cond
      [(null? s) '()]
      [else (cons (cons x (car s))
              (one-item x (cdr s)))])))
 
(define assqo
  (lambda (x ls out)
    (conde
     ;[(nullo ls) fail]
      [(exist (a b)
              (caro ls a)
              (caro a b)
              (== b x)
              (== out a))]
      ;[(eq? b x) (car ls)]
      [(exist (d)
              (cdro ls d)
              (assqo x d out))]
      ;[else (assq x (cdr ls))])))
      )))
      

(define assq
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(eq? (car (car ls)) x) (car ls)]
      [else (assq x (cdr ls))])))
 
(define reverseo
  (lambda (ls out)
    (conde
     [(nullo ls) (== out '())]
     [(exist (a d r)
             (caro ls a)
             (cdro ls d)
             (reverseo d r)
             (appendo r `(,a) out))]
     )))

(define reverse
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (append (reverse (cdr ls)) `(,(car ls)))])))


(define null?
  (lambda (p)
     (equal? p '())))

(define nullo
  (lambda (p)
    (== p '())))

(define member?
  (lambda (x ls)
    (cond
     [(null? ls) #f]
     [(eq? (car ls) x) #t]
     [else (member? x (cdr ls))])))

(define membero
  (lambda (x ls out)
    (conde
     [(exist (a)
             (caro ls a)
             (== a x))]
     [(exist (d)
             (cdro ls d)
             (membero x d out))]
     )))


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
 
(define-syntax multi-test
  (syntax-rules ()
    ((_ title tested-expression expected-result* ...)
     (let* ((expected* expected-result*)
            ...
            (produced tested-expression))
       (cond
         [(equal? produced expected-result*) (printf "~s works!\n" title)]
         ...
         [else (errorf
                'test
                "Failed ~s: ~a\nComputed: ~a"
                title 'tested-expression produced)])))))
 
(test "one-itemo-1"
  (run* (q)
    (one-itemo 'a '(b c d) q))
  '(((a . b) (a . c) (a . d))))
 
(test "one-itemo-2"
  (run* (q)
    (one-itemo q '(b c d) '((a . b) (a . c) (a . d))))
  '(a))
 
(test "one-itemo-3"
  (run* (q)
    (one-itemo 'a q '((a . b) (a . c) (a . d))))
  '((b c d)))
 
(test "one-itemo-4"
  (run* (q)
    (one-itemo 'a '(b c d) `((a . b) . ,q)))
  '(((a . c) (a . d))))
 
(test "one-itemo-5"
  (run* (q)
    (one-itemo 'a '(b c d) '((a . b) (a . c) (a . d))))
  '(_.0))
 
(test "one-itemo-6"
  (run* (q)
    (one-itemo 'a `(b ,q d) '((a . b) (a . c) (a . d))))
  '(c))
 
(test "one-itemo-7"
  (run* (q)
    (exist (x y)
      (one-itemo x y '((a . b) (a . c) (a . d)))
      (== `(,x ,y) q)))
  '((a (b c d))))
 
(test "one-itemo-8"
  (run 6 (q)
    (exist (x y z)
      (one-itemo x y z)
      (== `(,x ,y ,z) q)))
  '((_.0 () ()) (_.0 (_.1) ((_.0 . _.1)))
    (_.0 (_.1 _.2) ((_.0 . _.1) (_.0 . _.2)))
    (_.0 (_.1 _.2 _.3) ((_.0 . _.1) (_.0 . _.2) (_.0 . _.3)))
    (_.0 (_.1 _.2 _.3 _.4) ((_.0 . _.1) (_.0 . _.2) (_.0 . _.3) (_.0 . _.4)))
    (_.0 (_.1 _.2 _.3 _.4 _.5) ((_.0 . _.1) (_.0 . _.2) (_.0 . _.3) (_.0 . _.4) (_.0 . _.5)))))
 
(test "assqo-1"
  (run* (q)
    (assqo 'x '() q))
  '())
 
(test "assqo-2"
  (run* (q)
    (assqo 'x '((x . 5)) q))
  '((x . 5)))
 
(test "assqo-3"
  (run* (q)
    (assqo 'x '((y . 6) (x . 5)) q))
  '((x . 5)))
 
(test "assqo-4"
  (run* (q)
    (assqo 'x '((x . 6) (x . 5)) q))
  '((x . 6) (x . 5)))
 
(test "assqo-5"
  (run* (q)
    (assqo 'x '((x . 5)) '(x . 5)))
  '(_.0))
 
(test "assqo-6"
  (run* (q)
    (assqo 'x '((x . 6) (x . 5)) '(x . 6)))
  '(_.0))
 
(test "assqo-7"
  (run* (q)
    (assqo 'x '((x . 6) (x . 5)) '(x . 5)))
  '(_.0))
 
(test "assqo-8"
  (run* (q)
    (assqo q '((x . 6) (x . 5)) '(x . 5)))
  '(x))
 
(test "assqo-9"
  (run* (q)
    (assqo 'x '((x . 6) . ,q) '(x . 6)))
  '(_.0))
 
(multi-test "assqo-10"
  (run 10 (q)
    (assqo 'x q '(x . 5)))
  '(((x . 5) . _.0)
    (_.0 (x . 5) . _.1)
    (_.0 _.1 (x . 5) . _.2)
    (_.0 _.1 _.2 (x . 5) . _.3)
    (_.0 _.1 _.2 _.3 (x . 5) . _.4)
    (_.0 _.1 _.2 _.3 _.4 (x . 5) . _.5)
    (_.0 _.1 _.2 _.3 _.4 _.5 (x . 5) . _.6)
    (_.0 _.1 _.2 _.3 _.4 _.5 _.6 (x . 5) . _.7)
    (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 (x . 5) . _.8)
    (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 (x . 5) . _.9))
  '(((x . 5) . _.0)
    ((_.0 . _.1) (x . 5) . _.2)
    ((_.0 . _.1) (_.2 . _.3) (x . 5) . _.4)
    ((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (x . 5) . _.6)
    ((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (_.6 . _.7) (x . 5) . _.8)
    ((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (_.6 . _.7) (_.8 . _.9) (x . 5) . _.10)
    ((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (_.6 . _.7) (_.8 . _.9) (_.10 . _.11) (x . 5) . _.12)
    ((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (_.6 . _.7) (_.8 . _.9) (_.10 . _.11) (_.12 . _.13) (x . 5) . _.14)
    ((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (_.6 . _.7) (_.8 . _.9) (_.10 . _.11) (_.12 . _.13) (_.14 . _.15) (x . 5) . _.16)
    ((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (_.6 . _.7) (_.8 . _.9) (_.10 . _.11) (_.12 . _.13) (_.14 . _.15) (_.16 . _.17) (x . 5) . _.18)))
 
(multi-test "assqo-11"
  (run 10 (q)
    (exist (x y z)
      (assqo x y z)
      (== `(,x ,y ,z) q)))
  '((_.0 ((_.0 . _.1) . _.2) (_.0 . _.1))
    (_.0 (_.1 (_.0 . _.2) . _.3) (_.0 . _.2))
    (_.0 (_.1 _.2 (_.0 . _.3) . _.4) (_.0 . _.3))
    (_.0 (_.1 _.2 _.3 (_.0 . _.4) . _.5) (_.0 . _.4))
    (_.0 (_.1 _.2 _.3 _.4 (_.0 . _.5) . _.6) (_.0 . _.5))
    (_.0 (_.1 _.2 _.3 _.4 _.5 (_.0 . _.6) . _.7) (_.0 . _.6))
    (_.0 (_.1 _.2 _.3 _.4 _.5 _.6 (_.0 . _.7) . _.8) (_.0 . _.7))
    (_.0 (_.1 _.2 _.3 _.4 _.5 _.6 _.7 (_.0 . _.8) . _.9) (_.0 . _.8))
    (_.0 (_.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 (_.0 . _.9) . _.10) (_.0 . _.9))
    (_.0 (_.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 (_.0 . _.10) . _.11) (_.0 . _.10)))
  '((_.0
     ((_.0 . _.1) . _.2)
     (_.0 . _.1))
    (_.0
     ((_.1 . _.2) (_.0 . _.3) . _.4)
     (_.0 . _.3))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.0 . _.5) . _.6)
     (_.0 . _.5))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.0 . _.7) . _.8)
     (_.0 . _.7))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8) (_.0 . _.9) . _.10)
     (_.0 . _.9))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8) (_.9 . _.10) (_.0 . _.11) . _.12)
     (_.0 . _.11))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8)
      (_.9 . _.10) (_.11 . _.12) (_.0 . _.13) . _.14)
     (_.0 . _.13))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8) (_.9 . _.10) (_.11 . _.12) (_.13 . _.14) (_.0 . _.15) . _.16)
     (_.0 . _.15))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8) (_.9 . _.10) (_.11 . _.12) (_.13 . _.14) (_.15 . _.16) (_.0 . _.17) . _.18)
     (_.0 . _.17))
    (_.0
     ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8) (_.9 . _.10) (_.11 . _.12) (_.13 . _.14) (_.15 . _.16) (_.17 . _.18) (_.0 . _.19) . _.20)
     (_.0 . _.19))))

(test "reverseo-1"
  (run* (q) (reverseo '() q))
  '(()))
 
(test "reverseo-2"
  (run* (q) (reverseo '(a) q))
  '((a)))
 
(test "reverseo-3"
  (run* (q) (reverseo '(a b c d) q))
  '((d c b a)))
 
(test "reverseo-4"
  (run* (q) (exist (x) (reverseo `(a b ,x c d) q)))
  '((d c _.0 b a)))
 
(test "reverseo-5"
  (run* (x) (reverseo `(a b ,x d) '(d c b a)))
  '(c))
 
(test "reverseo-6"
  (run* (x) (reverseo `(a b c d) `(d . ,x)))
  '((c b a)))
 
(test "reverseo-7"
  (run* (q) (exist (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
  '(c))
 
(multi-test "reverseo-8"
  (run 10 (q) (exist (x y) (reverseo x y) (== `(,x ,y) q)))
  '((() ()) ((_.0) (_.0)) ((_.0 _.1) (_.1 _.0))
    ((_.0 _.1 _.2) (_.2 _.1 _.0))
    ((_.0 _.1 _.2 _.3) (_.3 _.2 _.1 _.0))
    ((_.0 _.1 _.2 _.3 _.4) (_.4 _.3 _.2 _.1 _.0))
    ((_.0 _.1 _.2 _.3 _.4 _.5) (_.5 _.4 _.3 _.2 _.1 _.0))
    ((_.0 _.1 _.2 _.3 _.4 _.5 _.6)
     (_.6 _.5 _.4 _.3 _.2 _.1 _.0))
    ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7)
     (_.7 _.6 _.5 _.4 _.3 _.2 _.1 _.0))
    ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8)
     (_.8 _.7 _.6 _.5 _.4 _.3 _.2 _.1 _.0))))