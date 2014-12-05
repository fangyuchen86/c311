(in-package "ACL2")

(include-book "data")

(defun member-p (x xs)
  (cond
    ((atom xs) 'nil)
    ((equal (car xs) x) 't)
    ('t (member-p x (cdr xs)))))

(defun removal (x xs)
  (cond
    ((atom xs) '())
    ((equal x (car xs)) (removal x (cdr xs)))
    ('t (cons (car xs) (removal x (cdr xs))))))

(defun list-insert (x xs)
  (cons x (removal x xs)))

(defun list-union (xs ys)
  (cond
    ((atom xs) ys)
    ('t (list-insert (car xs) (list-union (cdr xs) ys)))))

(defun get-arg-from (n args from)
  (cond
    ((atom args) 'nil)
    ('t (cond
          ((equal n from) (car args))
          ('t (get-arg-from n (cdr args)
		(+ from '1)))))))
(defun get-arg (n args)
  (get-arg-from n args '1))

(defun set-arg-from (n args y from)
  (cond
    ((atom args) '())
    ('t (cond
          ((equal n from) (cons y (cdr args)))
          ('t (cons (car args)
                (set-arg-from n (cdr args) y
		  (+ from '1))))))))
(defun set-arg (n args y)
  (set-arg-from n args y '1))

(defun <=len-from (n args from)
  (cond
    ((equal n from) 't)
    ((atom args) 'nil)
    ('t (<=len-from n (cdr args) (+ from '1)))))
(defun <=len (n args)
  (<=len-from n args '0))

(defun arity-check (vars es)
  (cond
    ((atom vars) (atom es))
    ((atom es) 'nil)
    ('t (arity-check (cdr vars) (cdr es)))))

(defun empty-prems ()
  (list2 '() '()))

(defun extend-prems-pos (e prems)
  (list2
    (list-union (list1 e) (1st prems))
    (2nd prems)))

(defun extend-prems-neg (e prems)
  (list2
    (1st prems)
    (list-union (list1 e) (2nd prems))))

(defun prems-pos-p (e prems)
  (cond
    ((member-p e (2nd prems)) 'nil)
    ((member-p e (1st prems)) 't)
    ('t 'nil)))

(defun prems-neg-p (e prems)
  (cond
    ((member-p e (1st prems)) 'nil)
    ((member-p e (2nd prems)) 't)
    ('t 'nil)))

(defun extend-prems (step e prems)
  (cond
    ((equal step 'A)
     (extend-prems-pos (cond.Q e) prems))
    ((equal step 'E)
     (extend-prems-neg (cond.Q e) prems))
    ('t prems)))

(defun follow-prems (prems e)
  (cond
    ((cond-p e)
     (cond
       ((prems-pos-p (cond.Q e) prems)
        (follow-prems prems (cond.A e)))
       ((prems-neg-p (cond.Q e) prems)
        (follow-prems prems (cond.E e)))
       ('t e)))
    ('t e)))
