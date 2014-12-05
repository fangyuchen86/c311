
(define five '(dethm five (equal (+ '2 '3) '5)))
(define five-acts
  '((equ (1) (+ '2 '3))
    (equ () (equal-same '5))
    ))

(define ten '(dethm ten (equal (+ (+ '2 '3) (+ '2 '3)) '10)))
(define ten-acts
  '((equ (1 1) (five))
    (equ (1 2) (five))
    (equ (1) (+ '5 '5))
    (equ () (equal-same '10))
    ))

(define lessthan '(dethm lessthan (< (+ '2 '5) (+ '3 '5))))
(define lessthan-acts
  '((equ (1) (+ '2 '5))
    (equ (2) (+ '3 '5))
    (equ () (< '7 '8))
    ))


(define cond-sevens '(dethm cond-sevens (equal (cond
                                                 [(< '5 '6) '7]
                                                 ['t (+ '6 '1)])
                                               '7)))
(define cond-sevens-acts-1
  '((equ (1 Q) (< '5 '6))
    (equ (1) (cond-question-true '7 (+ '6 '1)))
    (equ () (equal-same '7))
    ))


(define cond-sevens-acts-2
  '((equ (1 Q) (< '5 '6))
    (equ (1 E) (+ '6 '1))
    (equ (1) (cond-answers-same 't '7))
    (equ () (equal-same '7))
    ))

(define cond-sevens-acts-3
  '((equ (1 Q) (< '5 '6))
    (equ (1 E) (+ '6 '1))
    (equ (1) (cond-opposite 't '7 '7))
    (equ (1 Q) (equal 't 'nil))
    (equ (1) (cond-question-false '7 '7))
    (equ () (equal-same '7))
    ))


(define car-warmup '(dethm car-warmup (equal (car (cons a d)) a)))
(define car-warmup-acts
  '((equ (1) (car-cons a d))
    (equ () (equal-same a))
    ))

(define cadar-thm '(dethm cadar
                     (equal (car (cdr (car (cons (cons aa (cons ada add)) d))))
                            ada)))
(define cadar-acts
  '((equ (1 1 1) (car-cons (cons aa (cons ada add)) d))
    (equ (1 1) (cdr-cons aa (cons ada add)))
    (equ (1) (car-cons ada add))
    (equ () (equal-same ada))
    ))

(define use-cadar-thm '(dethm use-cadar
                         (equal (car (cdr (car (cons
                                                 (cons '1
                                                       (cons '2 '3)) 
                                                 '4))))
                                '2)))

(define use-cadar-acts
  '((equ (1) (cadar '1 '2 '3 '4))
    (equ () (equal-same '2))
    ))



(define fun (J-Bob/fun+ (axioms/theorems)
           '(defun mem-p (xs)
              (cond
                ((atom xs) 'nil)
                ('t (cond
                      ((equal (car xs) '?) 't)
                      ('t (mem-p (cdr xs)))))))
           '(acl2-count xs)
           '((equ (A E E 2 1) (cons-car/cdr xs))
             (equ (A E E) (acl2-count-cdr (cdr xs) (car xs)))
             (equ (A E) (cond-answers-same (equal (car xs) '?) 't))
             (equ (A) (cond-answers-same (atom xs) 't))
             (equ (Q) (acl2-count-natural xs))
             (equ () (cond-question-true 't 'nil)))))
 
(define ax (J-Bob/fun+ fun
           '(defun rem (xs)
              (cond
                ((atom xs) '())
                ('t (cond
                      ((equal (car xs) '?) (rem (cdr xs)))
                      ('t (cons (car xs)
                                (rem (cdr xs))))))))
           '(acl2-count xs)
           '((equ (A E 2 1) (cons-car/cdr xs))
             (equ (A E) (acl2-count-cdr (cdr xs) (car xs)))
             (equ (A) (cond-answers-same (atom xs) 't))
             (equ (Q) (acl2-count-natural xs))
             (equ () (cond-question-true 't 'nil))
             )))

(define mem-p/rem0 '(dethm mem-p/rem0
                           (equal (mem-p (rem '())) 'nil)))
(define mem-p/rem0-acts
  '((equ (1 1) (rem '()))
    (equ (1 1 E Q 1) (car '()))
    (equ (1 1 E Q) (equal 'nil '?))
    (equ (1 1 E) (cond-question-false (rem (cdr '())) (cons (car '()) (rem (cdr '())))))
    (equ (1 1 Q) (atom '()))
    (equ (1 1) (cond-question-true '() (cons (car '()) (rem (cdr '())))))
    (equ (1) (mem-p '()))
    (equ (1 Q) (atom '()))
    (equ (1) (cond-question-true 'nil (cond [(equal (car '()) '?) 't] ['t (mem-p (cdr '()))])))
    (equ () (equal-same 'nil))
    ))
    


(define mem-p/rem1 '(dethm mem-p/rem1
                           (equal (mem-p (rem (cons x1 '()))) 'nil)))









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
 
(test 'five (J-Bob/thm (axioms/theorems) five five-acts) ''t)
 
(let ([defs (J-Bob/thm+ (axioms/theorems) five five-acts)])
  (test 'ten (J-Bob/thm defs ten ten-acts) ''t))
 
(test 'lessthan (J-Bob/thm (axioms/theorems) lessthan lessthan-acts) ''t)
 
(test 'cond-sevens-1 (J-Bob/thm (axioms/theorems) cond-sevens cond-sevens-acts-1) ''t)
(test 'cond-sevens-2 (J-Bob/thm (axioms/theorems) cond-sevens cond-sevens-acts-2) ''t)
(test 'cond-sevens-3 (J-Bob/thm (axioms/theorems) cond-sevens cond-sevens-acts-3) ''t)
 
(test 'car-warmup (J-Bob/thm (axioms/theorems)
                             car-warmup
                             car-warmup-acts)
      ''t)
 
(test 'cadar (J-Bob/thm (axioms/theorems)
                        cadar-thm
                        cadar-acts)
      ''t)
 
(let ([defs (J-Bob/thm+ (axioms/theorems)
                        cadar-thm
                        cadar-acts)])
  (test 'use-cadar (J-Bob/thm defs use-cadar-thm use-cadar-acts) ''t))
 
(let* ([defs (J-Bob/fun+ (axioms/theorems)
                         '(defun mem-p (xs)
                            (cond
                              ((atom xs) 'nil)
                              ('t (cond
                                    ((equal (car xs) '?) 't)
                                    ('t (mem-p (cdr xs)))))))
                         '(acl2-count xs)
                         '((equ (A E E 2 1) (cons-car/cdr xs))
                           (equ (A E E) (acl2-count-cdr (cdr xs) (car xs)))
                           (equ (A E) (cond-answers-same (equal (car xs) '?) 't))
                           (equ (A) (cond-answers-same (atom xs) 't))
                           (equ (Q) (acl2-count-natural xs))
                           (equ () (cond-question-true 't 'nil))))]
       [defs (J-Bob/fun+ defs
                         '(defun rem (xs)
                            (cond
                              ((atom xs) '())
                              ('t (cond
                                    ((equal (car xs) '?) (rem (cdr xs)))
                                    ('t (cons (car xs)
                                              (rem (cdr xs))))))))
                         '(acl2-count xs)
                         '((equ (A E 2 1) (cons-car/cdr xs))
                           (equ (A E) (acl2-count-cdr (cdr xs) (car xs)))
                           (equ (A) (cond-answers-same (atom xs) 't))
                           (equ (Q) (acl2-count-natural xs))
                           (equ () (cond-question-true 't 'nil))))])
  (test 'mem-p/rem0 (J-Bob/thm defs
                               mem-p/rem0
                               mem-p/rem0-acts)
        ''t)
  (let ([defs (J-Bob/thm+ defs
                          mem-p/rem0
                          mem-p/rem0-acts)])
    (test 'mem-p/rem1 (J-Bob/thm defs
                                 mem-p/rem1
                                 mem-p/rem1-acts)
          ''t)))