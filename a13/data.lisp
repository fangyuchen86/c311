(in-package "ACL2")

(defun list0 () '())
(defun list0-p (x) (equal x '()))

(defun list1 (x) (cons x (list0)))
(defun list1-p (x)
  (cond
    ((atom x) 'nil)
    ('t (list0-p (cdr x)))))
(defun 1st (xs) (car xs))

(defun list2 (x y) (cons x (list1 y)))
(defun list2-p (x)
  (cond
    ((atom x) 'nil)
    ('t (list1-p (cdr x)))))
(defun 2nd (xs) (1st (cdr xs)))

(defun list3 (x y z) (cons x (list2 y z)))
(defun list3-p (x)
  (cond
    ((atom x) 'nil)
    ('t (list2-p (cdr x)))))
(defun 3rd (xs) (2nd (cdr xs)))

(defun tag (sym x) (cons sym x))
(defun tag-p (sym x)
  (cond
    ((atom x) 'nil)
    ('t (equal (car x) sym))))
(defun untag (x) (cdr x))

(defun defun-c (name formals body)
  (tag 'defun (list3 name formals body)))
(defun defun-p (x)
  (cond
    ((tag-p 'defun x) (list3-p (untag x)))
    ('t 'nil)))
(defun defun.name (def) (1st (untag def)))
(defun defun.formals (def) (2nd (untag def)))
(defun defun.body (def) (3rd (untag def)))

(defun dethm-c (name body)
  (tag 'dethm (list2 name body)))
(defun dethm-p (x)
  (cond
    ((tag-p 'dethm x) (list2-p (untag x)))
    ('t 'nil)))
(defun dethm.name (def) (1st (untag def)))
(defun dethm.body (def) (2nd (untag def)))

(defun quote-c (value)
  (tag 'quote (list1 value)))
(defun quote-p (x)
  (cond
    ((tag-p 'quote x)
     (list1-p (untag x)))
    ('t 'nil)))
(defun quote.value (e) (1st (untag e)))

(defun cond-c (Q A E)
  (tag 'cond
    (list2 (list2 Q A)
           (list2 (quote-c 't) E))))
(defun cond-p (x)
  (cond
    ((tag-p 'cond x)
     (cond
       ((list2-p (untag x))
        (cond
          ((list2-p (1st (untag x)))
           (list2-p (2nd (untag x))))
          ('t 'nil)))
       ('t 'nil)))
    ('t 'nil)))
(defun cond.Q (e) (1st (1st (untag e))))
(defun cond.A (e) (2nd (1st (untag e))))
(defun cond.E (e) (2nd (2nd (untag e))))

(defun app-c (name args) (cons name args))
(defun app-p (x)
  (cond
    ((atom x) 'nil)
    ((quote-p x) 'nil)
    ((cond-p x) 'nil)
    ('t 't)))
(defun app.name (e) (car e))
(defun app.args (e) (cdr e))

(defun equ-c (path app)
  (tag 'equ (list2 path app)))
(defun equ-p (x)
  (cond
    ((tag-p 'equ x) (list2-p (untag x)))
    ('t 'nil)))
(defun equ.path (act) (1st (untag act)))
(defun equ.app (act) (2nd (untag act)))

(defun ind-c (app)
  (tag 'ind (list1 app)))
(defun ind-p (x)
  (cond
    ((tag-p 'ind x) (list1-p (untag x)))
    ('t 'nil)))
(defun ind.app (act) (1st (untag act)))
