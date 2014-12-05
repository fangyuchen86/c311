(in-package "ACL2")

(include-book "checks")

(defun find-focus-at-step (step e)
  (cond
    ((equal step 'Q) (cond.Q e))
    ((equal step 'A) (cond.A e))
    ((equal step 'E) (cond.E e))
    ('t (get-arg step (app.args e)))))

(defun rewrite-focus-at-step (step e1 e2)
  (cond
    ((equal step 'Q)
     (cond-c e2 (cond.A e1) (cond.E e1)))
    ((equal step 'A)
     (cond-c (cond.Q e1) e2 (cond.E e1)))
    ((equal step 'E)
     (cond-c (cond.Q e1) (cond.A e1) e2))
    ('t (app-c (app.name e1)
          (set-arg step (app.args e1) e2)))))

(defun step-check (step e)
  (cond
    ((equal step 'Q) (cond-p e))
    ((equal step 'A) (cond-p e))
    ((equal step 'E) (cond-p e))
    ((natp step)
     (cond
       ((< '0 step)
        (cond
          ((app-p e) (<=len step (app.args e)))
          ('t 'nil)))
       ('t 'nil)))
    ('t 'nil)))

(defun equal-check (e)
  (cond
    ((app-p e)
     (cond
       ((equal 'equal (app.name e))
        (arity-check '(x y) (app.args e)))
       ('t 'nil)))
    ('t 'nil)))
