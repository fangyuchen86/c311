(in-package "ACL2")

(include-book "j-bob")

(defun axioms ()
  '((dethm car-cons
      (equal (car (cons x y)) x))
    (dethm cdr-cons
      (equal (cdr (cons x y)) y))
    (dethm equal-same
      (equal (equal x x) 't))
    (dethm equal-symmetry
      (equal (equal x y) (equal y x)))
    (dethm equal-conditional
      (cond ((equal x y) (equal x y)) ('t 't)))
    (dethm cond-question-false
      (equal (cond ('nil x) ('t y)) y))
    (dethm cond-answers-same
      (equal (cond (x y) ('t y)) y))
    (dethm cond-opposite
      (equal (cond ((equal x 'nil) y) ('t z))
             (cond (x z) ('t y))))
    (dethm atom-cons
      (equal (atom (cons x y)) 'nil))
    (dethm cons-car/cdr
      (cond
        ((atom x) 't)
        ('t (equal (cons (car x) (cdr x)) x))))
    (dethm acl2-count-car
      (equal (< (acl2-count x) (acl2-count (cons x y))) 't))
    (dethm acl2-count-cdr
      (equal (< (acl2-count y) (acl2-count (cons x y))) 't))
    (dethm acl2-count-natural
      (equal (natp (acl2-count x)) 't))
    (dethm +-identity
      (cond ((natp x) (equal (+ '0 x) x)) ('t 't)))
    (dethm +-commutativity
      (equal (+ x y) (+ y x)))
    (dethm +-associativity
      (equal (+ x (+ y z)) (+ (+ x y) z)))
    (dethm +-natural
      (cond
        ((natp x)
         (cond
           ((natp y) (equal (natp (+ x y)) 't))
           ('t 't)))
        ('t 't)))
    (dethm <-addition
      (equal (< (+ x z) (+ y z)) (< x y)))
    (dethm <-positive
      (cond
        ((< '0 x)
         (cond
           ((< '0 y) (equal (< '0 (+ x y)) 't))
           ('t 't)))
        ('t 't)))))

(defun axioms+1 ()
  (J-Bob/thm+ (axioms)
    '(dethm cond-question-true
       (equal (cond ('t x) ('t y)) x))
    '((equ (1) (cond-opposite 't y x))
      (equ (1 Q) (equal 't 'nil))
      (equ (1) (cond-question-false y x))
      (equ () (equal-same x)))))

(defun axioms+2 ()
  (J-Bob/thm+ (axioms+1)
    '(dethm cond-else-nil
       (cond
         (x 't)
         ('t (equal x 'nil))))
    '((equ () (cond-opposite x (equal x 'nil) 't))
      (equ (A 1) (equal-conditional x 'nil))
      (equ (A) (equal-same 'nil))
      (equ ()
        (cond-answers-same (equal x 'nil) 't)))))

(defun axioms+3 ()
  (J-Bob/thm+ (axioms+2)
    '(dethm cond-nesting-else
       (cond
         (x 't)
         ('t (equal (cond (x y) ('t z)) z))))
    '((equ (E 1 Q) (cond-else-nil x))
      (equ (E 1) (cond-question-false y z))
      (equ (E) (equal-same z))
      (equ () (cond-answers-same x 't)))))

(defun axioms/theorems ()
  (J-Bob/thm+ (axioms+3)
    '(dethm cond-nesting-answer
       (cond
         (x (equal (cond (x y) ('t z)) y))
         ('t 't)))
    '((equ ()
        (cond-opposite
          x
          't
          (equal (cond (x y) ('t z)) y)))
      (equ (E 1) (cond-opposite x z y))
      (equ (E 1)
        (cond-nesting-else (equal x 'nil) z y))
      (equ (E) (equal-same y))
      (equ ()
        (cond-answers-same (equal x 'nil) 't)))))
